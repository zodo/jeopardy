package zodo.jeopardy.client

import zio.{Has, Layer}
import zio.config._
import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.config.typesafe.TypesafeConfig
import zodo.jeopardy.client.AppConfig.ServerConfig
import zodo.jeopardy.model.GameConfig

import java.io.File

case class AppConfig(
  game: GameConfig,
  server: ServerConfig
)

object AppConfig {
  case class ServerConfig(packContentPath: String)

  val description: ConfigDescriptor[AppConfig] = descriptor[AppConfig]

  val live: Layer[ReadError[String], Has[AppConfig]] =
    TypesafeConfig
      .fromHoconFile(new File("application.conf"), description)
      .orElse(TypesafeConfig.fromDefaultLoader(description))
}
