package helpers

import java.math.BigInteger
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.appkit.{Address, NetworkType}

object Configs extends ConfigHelper {
  lazy val nodeUrl: String = readKey("node.url")
  lazy val nodeApiKey: String = readKey("node.apiKey", "")
  lazy val proxySecret: BigInteger = BigInt(readKey("proxy.secret"), 16).bigInteger
  lazy val proxyAddress: Address = Address.create(readKey("proxy.address"))
  lazy val networkType: NetworkType = if (readKey("node.networkType").toLowerCase.equals("mainnet")) NetworkType.MAINNET else NetworkType.TESTNET
  lazy val addressEncoder = new ErgoAddressEncoder(networkType.networkPrefix)

  lazy val explorerUrl: String = readKey("explorer.url")
  lazy val signalBoxValue: Long =  1000000L
  lazy val defaultTxFee: Long =  1000000L
}