package helpers

import com.typesafe.config.ConfigFactory
import play.api.{Configuration, Logger}

trait ConfigHelper {
  val config: Configuration = Configuration(ConfigFactory.load())
  private val logger: Logger = Logger(this.getClass)

  /**
   * Read the config and return the value of the key
   *
   * @param key     key to find
   * @param default default value if the key is not found
   * @return value of the key
   */
  def readKey(key: String, default: String = null): String = {
    try {
      if (default != null && !config.has(key)) return default
      if (config.has(key)) config.getOptional[String](key).getOrElse(default)
      else throw config.reportError(key, s"${key} is required.")
    } catch {
      case ex: Throwable =>
        logger.error(ex.getMessage)
        sys.exit()
    }
  }

  def readSeq(key: String): Seq[String] = {
    try {
      if (config.has(key)) config.getOptional[Seq[String]](key).getOrElse(Seq())
      else throw config.reportError(key, s"$key is required.")
    } catch {
      case ex: Throwable =>
        logger.error(ex.getMessage)
        sys.exit()
    }
  }
}