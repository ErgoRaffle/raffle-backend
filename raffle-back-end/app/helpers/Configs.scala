package helpers

import java.math.BigInteger
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.appkit.{Address, NetworkType}

object Configs extends ConfigHelper {
  lazy val nodeUrl: String = readKey("node.url")
  lazy val nodeApiKey: String = readKey("node.apiKey", "")
  lazy val networkType: NetworkType = if (readKey("node.networkType").toLowerCase.equals("mainnet")) NetworkType.MAINNET else NetworkType.TESTNET
  lazy val addressEncoder = new ErgoAddressEncoder(networkType.networkPrefix)

  lazy val explorerUrl: String = readKey("explorer.url")
  lazy val defaultTxFee: Long =  1000000L

  lazy val fee: Long = readKey("fee").toLong
  lazy val dummyTxId: String = readKey("dummyTxId")
  lazy val serviceSecret: BigInteger = BigInt(readKey("service.secret"), 16).bigInteger
  lazy val serviceAddress: Address = Address.create(readKey("service.address"))
  lazy val serviceTokenId: String = readKey("service.token")
  lazy val raffleProjectAddress: Address = Address.create(readKey("raffle.project.address"))
  lazy val oracleId: String = readKey("oracleId")
  lazy val servicePercent: Long = readKey("service.percent").toLong

  lazy val creationDelay: Int = readKey("creationDelay").toInt
  lazy val checkingDelay: Int = readKey("checkingDelay").toInt
  lazy val inf: Int = readKey("inf").toInt
  lazy val infBoxVal: Long = readKey("infBoxVal").toLong

  lazy val creationThreadInterval: Int = readKey("creationThreadInterval").toInt
  lazy val donateThreadInterval: Int = readKey("donateThreadInterval").toInt
  lazy val refundThreadInterval: Int = readKey("refundThreadInterval").toInt
  lazy val raffleThreadInterval: Int = readKey("raffleThreadInterval").toInt
  lazy val searchThreadInterval: Int = readKey("searchThreadInterval").toInt

  object token {
    lazy val nft: String = readKey("raffle.token.nft")
    lazy val service: String = readKey("raffle.token.service")
    lazy val oracle: String = readKey("raffle.token.oracle")
  }
}
