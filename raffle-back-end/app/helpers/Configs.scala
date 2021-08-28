package helpers

import java.math.BigInteger
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.appkit.{Address, NetworkType}

object Configs extends ConfigHelper {
  lazy val nodeUrl: String = readKey("node.url").replaceAll("/$", "")
  lazy val networkType: NetworkType = if (readKey("node.networkType").toLowerCase.equals("mainnet")) NetworkType.MAINNET else NetworkType.TESTNET
  lazy val addressEncoder = new ErgoAddressEncoder(networkType.networkPrefix)
  lazy val explorerUrl: String = readKey("explorer.url").replaceAll("/$", "")
  lazy val explorerFront: String = readKey("explorer.front").replaceAll("/$", "")

  lazy val fee: Long = readKey("fee").toLong
  lazy val minBoxErg: Long = readKey("minBoxErg").toLong
  lazy val serviceAddress: Address = Address.create(readKey("service.address"))

  lazy val creationDelay: Int = readKey("creationDelay").toInt
  lazy val checkingDelay: Int = readKey("checkingDelay").toInt
  lazy val inf: Int = readKey("inf").toInt
  lazy val infBoxVal: Long = readKey("infBoxVal").toLong

  lazy val creationThreadInterval: Int = readKey("creationThreadInterval").toInt
  lazy val donateThreadInterval: Int = readKey("donateThreadInterval").toInt
  lazy val refundThreadInterval: Int = readKey("refundThreadInterval").toInt

  lazy val recaptchaKey: String = readKey("recaptchaKey", default = "not-set")
  lazy val recaptchaPubKey: String = readKey("recaptchaPubKey", default = "not-set")

  lazy val activeFinalize: Boolean = readKey("activeFinalize").toBoolean

  object token {
    lazy val nft: String = readKey("raffle.token.nft")
    lazy val service: String = readKey("raffle.token.service")
    lazy val oracle: String = readKey("raffle.token.oracle")
  }
}
