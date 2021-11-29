package helpers

import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.appkit.{Address, NetworkType}

object Configs extends ConfigHelper {
  lazy val nodeUrl: String = readKey("node.url").replaceAll("/$", "")
  lazy val networkType: NetworkType = if (readKey("node.networkType").toLowerCase.equals("mainnet")) NetworkType.MAINNET else NetworkType.TESTNET
  lazy val addressEncoder = new ErgoAddressEncoder(networkType.networkPrefix)
  lazy val explorerUrl: String = readKey("explorer.url").replaceAll("/$", "")
  lazy val explorerFront: String = readKey("explorer.front").replaceAll("/$", "")

  lazy val ipfsPrefix: String = readKey("ipfsPrefix").replaceAll("/$", "")
  lazy val ipfsResolver: String = readKey("ipfsResolver").replaceAll("/$", "")

  lazy val fee: Long = readKey("fee").toLong
  lazy val creationFee: Long = fee * 4
  lazy val minBoxErg: Long = readKey("minBoxErg").toLong
  lazy val serviceOwner: Address = Address.create(readKey("service.owner"))
  lazy val serviceFeeAddress: Address = Address.create(readKey("service.feeAddress"))

  lazy val expireHeight: Long = readKey("expireHeight").toLong
  lazy val creationDelay: Int = readKey("creationDelay").toInt
  lazy val infBoxVal: Long = readKey("infBoxVal").toLong

  lazy val creationThreadInterval: Int = readKey("creationThreadInterval").toInt
  lazy val donateThreadInterval: Int = readKey("donateThreadInterval").toInt
  lazy val refundThreadInterval: Int = readKey("refundThreadInterval").toInt
  lazy val updateThreadInterval: Int = readKey("updateThreadInterval").toInt

  lazy val recaptchaKey: String = readKey("recaptchaKey", default = "not-set")
  lazy val recaptchaPubKey: String = readKey("recaptchaPubKey", default = "not-set")

  lazy val activeFinalize: Boolean = readKey("activeFinalize").toBoolean

  lazy val contactWebHook: String = readKey("contactWebHook")
  lazy val supportUrl: String = readKey("supportUrl")

  object token {
    lazy val nft: String = readKey("raffle.token.nft")
    lazy val service: String = readKey("raffle.token.service")
    lazy val oracle: String = readKey("raffle.token.oracle")
  }
}
