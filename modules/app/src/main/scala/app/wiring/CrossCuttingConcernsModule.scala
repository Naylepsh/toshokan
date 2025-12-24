package app.wiring

import java.net.InetAddress

import cats.effect.{Deferred, IO, Resource}
import com.microsoft.playwright.Browser
import doobie.Transactor
import okhttp3.OkHttpClient
import okhttp3.dnsoverhttps.DnsOverHttps
import scraper.util.playwright
import sttp.capabilities.WebSockets
import sttp.client3.SttpBackend
import sttp.client3.okhttp.OkHttpSyncBackend

case class InfrastructureResources(
    xa: Transactor[IO],
    httpBackend: SttpBackend[IO, WebSockets],
    browser: Browser,
    shutdownSignal: Deferred[IO, Unit]
)

object CrossCuttingConcernsModule:
  def setupResources(
      dbConfig: db.Config,
      useDnsOverHttps: Boolean
  ): Resource[IO, InfrastructureResources] =
    for
      xa          <- db.transactors.makeSqliteTransactorResource[IO](dbConfig)
      httpBackend <- createHttpBackend(useDnsOverHttps)
      browser <- playwright
        .makePlaywrightResource[IO]
        .evalMap(p => IO.delay(p.chromium().launch()))
      shutdownSignal <- Resource.eval(Deferred[IO, Unit])
    yield InfrastructureResources(xa, httpBackend, browser, shutdownSignal)

  private def createHttpBackend(
      useDnsOverHttps: Boolean
  ): Resource[IO, SttpBackend[IO, WebSockets]] =
    if useDnsOverHttps then
      // Use DNS-over-HTTPS to circumvent Cisco Umbrella
      Resource.make(IO.blocking {
        val dohClient = new OkHttpClient.Builder()
          .dns(
            new DnsOverHttps.Builder()
              .client(new OkHttpClient())
              .url(okhttp3.HttpUrl.parse("https://1.1.1.1/dns-query"))
              .bootstrapDnsHosts(InetAddress.getByName("1.1.1.1"))
              .build()
          )
          .build()

        val syncBackend = OkHttpSyncBackend.usingClient(dohClient)

        // Simple wrapper that converts sync calls to IO
        new SttpBackend[IO, WebSockets]:
          override def send[T, R >: WebSockets & sttp.capabilities.Effect[IO]](
              request: sttp.client3.Request[T, R]
          ): IO[sttp.client3.Response[T]] =
            IO.blocking(
              syncBackend.send(
                request.asInstanceOf[sttp.client3.Request[T, Any]]
              )
            )

          override def close(): IO[Unit] = IO.blocking(syncBackend.close())

          override def responseMonad: sttp.monad.MonadError[IO] =
            new sttp.monad.MonadError[IO]:
              override def unit[T](t: T): IO[T] = IO.pure(t)
              override def map[T, T2](fa: IO[T])(f: T => T2): IO[T2] = fa.map(f)
              override def flatMap[T, T2](fa: IO[T])(f: T => IO[T2]): IO[T2] =
                fa.flatMap(f)
              override def error[T](t: Throwable): IO[T] = IO.raiseError(t)
              override protected def handleWrappedError[T](rt: IO[T])(
                  h: PartialFunction[Throwable, IO[T]]
              ): IO[T] =
                rt.handleErrorWith(h.orElse { case e => IO.raiseError(e) })
              override def eval[T](t: => T): IO[T]        = IO.delay(t)
              override def suspend[T](t: => IO[T]): IO[T] = IO.defer(t)
              override def ensure[T](f: IO[T], e: => IO[Unit]): IO[T] =
                f.guarantee(e)
      })(_.close())
    else
      // Use standard HTTP client
      import sttp.client3.httpclient.cats.HttpClientCatsBackend
      HttpClientCatsBackend.resource[IO]()
