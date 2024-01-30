package library

import cats.Id
import library.domain.NewAsset
import library.domain.AssetTitle
import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource

@main
def run: Unit =
  val config = HikariConfig()
  config.setDriverClassName("org.sqlite.JDBC")
  config.setJdbcUrl("jdbc:sqlite:///home/naylepsh/dev/toshokan/db.sqlite")
  config.setUsername("")
  config.setPassword("")
  val ds    = HikariDataSource(config)
  val repo  = AssetRepository.make[Id, Int, Int](ds)
  val asset = NewAsset(AssetTitle("bar"))
  val result = repo.add(asset)
  println(result)
