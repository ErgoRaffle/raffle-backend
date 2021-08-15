package helpers

import java.io.{PrintWriter, StringWriter}
import javax.inject.{Inject, Singleton}
import com.typesafe.config.ConfigFactory
import org.ergoplatform.appkit.{ErgoType, ErgoValue, JavaHelpers}
import special.collection.Coll

import java.security.MessageDigest

@Singleton
class Utils @Inject()() {

  def getStackTraceStr(e: Throwable): String = {
    val sw = new StringWriter
    val pw = new PrintWriter(sw)
    e.printStackTrace(pw)
    sw.toString
  }

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

  def show(x: Option[String]) = x match {
    case Some(s) => s
    case None => "?"
  }

  def longListToErgoValue(elements: Array[Long]): ErgoValue[Coll[Long]] = {
    val longColl = JavaHelpers.SigmaDsl.Colls.fromArray(elements)
    ErgoValue.of(longColl, ErgoType.longType())
  }

}
