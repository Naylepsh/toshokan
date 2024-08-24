import cats.effect.IOApp
import cats.effect.IO
import cats.effect.kernel.Clock
// import scala.concurrent.duration.MILLISECONDS

object Main extends IOApp.Simple:
  override def run: IO[Unit] = 
    Clock[IO].realTime.map: now =>
      val nowMillis = now.toMillis
      val expiresIn = 2678400
      println(nowMillis + expiresIn * 1000)
