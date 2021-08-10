package helpers

import javax.inject.{Inject, Singleton}
import com.typesafe.config.ConfigFactory
import java.security.MessageDigest

@Singleton
class Utils @Inject()() {

  def getAPILink(APIName: String): String = {
//    return ConfigFactory.load().getString(s"API.${APIName}")
    return ConfigFactory.load().getString("API.unspentBoxesAPILink")
  }

  def getTokenId(): String = {
    return ConfigFactory.load().getString("Token.raffleCreator")
  }

  def getBoxCreator(): String = {
    return ConfigFactory.load().getString("Box.creatorBoxId")
  }

  def getOracleId(): String = {
    return ConfigFactory.load().getString("Box.oracleId")
  }

  def SHA1(input: String): String = {
    return MessageDigest
      .getInstance("SHA-1")
      .digest(input.getBytes("UTF-8"))
      .map("%02x".format(_))
      .mkString
  }
}