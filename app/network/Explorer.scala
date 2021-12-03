package network

import helpers.{Configs, connectionException, requestException, parseException}
import io.circe.Json

import javax.inject.Singleton
import org.ergoplatform.appkit.{Address, ErgoTreeTemplate}
import play.api.Logger

import sigmastate.Values.ErgoTree

@Singleton
class Explorer() {
  private val baseUrlV0 = s"${Configs.explorerUrl}/api/v0"
  private val baseUrlV1 = s"${Configs.explorerUrl}/api/v1"
  private val tx = s"$baseUrlV1/transactions"
  private val unconfirmedTx = s"$baseUrlV0/transactions/unconfirmed"
  private val unspentBoxesByTokenId = s"$baseUrlV1/boxes/unspent/byTokenId"
  private val allBoxesByTokenId = s"$baseUrlV1/boxes/byTokenId"
  private val boxesP1 = s"$tx/boxes"
  private val mempoolTransactions = s"$baseUrlV1/mempool/transactions/byAddress"
  private val boxSearch = s"$baseUrlV1/boxes/search"
  private val logger: Logger = Logger(this.getClass)

  /**
   * @param address address to search in mempool
   * @return mempool transactions belonging to the address
   */
  def getTxsInMempoolByAddress(address: String): Json = try {
    Request.httpGet(s"$mempoolTransactions/$address")
  } catch {
    case e: requestException =>
      logger.warn(e.getMessage)
      throw connectionException()
    case e: Throwable =>
      logger.error(e.getMessage)
      throw connectionException()
  }

  /**
   * @param txId transaction id
   * @return transaction if it is unconfirmed
   */
  def getUnconfirmedTx(txId: String): Json = try {
    Request.httpGet(s"$unconfirmedTx/$txId")
  } catch {
    case _: Throwable =>
      Json.Null
  }

  /**
   * @param txId transaction id
   * @return transaction if it is confirmed (mined)
   */
  def getConfirmedTx(txId: String): Json = try {
    Request.httpGet(s"$tx/$txId")
  } catch {
    case _: Throwable =>
      Json.Null
  }

  // TODO: use this function instead of utils.checkTransaction
  /**
   * @param txId transaction id
   * @return -1 if tx does not exist, 0 if it is unconfirmed, otherwise, confirmation num
   */
  def getConfNum(txId: String): Int = try {
    val unc = getUnconfirmedTx(txId)
    if (unc != Json.Null) 0
    else {
      val conf = getConfirmedTx(txId)
      if (conf != Json.Null) conf.hcursor.downField("summary").as[Json].getOrElse(throw parseException())
        .hcursor.downField("confirmationsCount").as[Int].getOrElse(-1)
      else -1
    }
  } catch {
    case e: connectionException => throw e
    case e: parseException =>
      logger.error(e.getMessage)
      throw connectionException()
    case e: Throwable =>
      logger.error(e.getMessage)
      throw connectionException()
  }

  def getUnspentTokenBoxes(tokenId: String, offset: Int, limit: Int): Json = try {
    Request.httpGet(s"$unspentBoxesByTokenId/$tokenId?offset=$offset&limit=$limit")
  } catch {
    case e: requestException =>
      logger.warn(e.getMessage)
      throw connectionException()
    case e: Throwable =>
      logger.error(e.getMessage)
      throw connectionException()
  }

  def getAllTokenBoxes(tokenId: String, offset: Int, limit: Int): Json = try {
    Request.httpGet(s"$allBoxesByTokenId/$tokenId?offset=$offset&limit=$limit")
  } catch {
    case e: requestException =>
      logger.warn(e.getMessage)
      throw connectionException()
    case e: Throwable =>
      logger.error(e.getMessage)
      throw connectionException()
  }

  def getUnspentBoxByID(boxId: String): Json = try {
    Request.httpGet(s"$boxesP1/$boxId")
  } catch {
    case e: requestException =>
      logger.warn(e.getMessage)
      throw connectionException()
    case e: Throwable =>
      logger.error(e.getMessage)
      throw connectionException()
  }

  def getUnconfirmedTxByAddress(address: String): Json = try {
    Request.httpGet(s"$unconfirmedTx/byAddress/$address/?offset=0&limit=100")
  } catch {
    case e: requestException =>
      logger.warn(e.getMessage)
      throw connectionException()
    case e: Throwable =>
      logger.error(e.getMessage)
      throw connectionException()
  }

  def getBoxesByErgoTree(ergoTree: ErgoTree, tokenId: String): Json = try {
    val ergoTreeTemplateHash = scorex.crypto.hash.Sha256(ErgoTreeTemplate.fromErgoTree(ergoTree).getBytes).map("%02x".format(_)).mkString
    val json = Json.fromFields(List(
      ("ergoTreeTemplateHash", Json.fromString(ergoTreeTemplateHash)),
      ("assets", Json.fromValues(Array(Json.fromString(tokenId)))),
    ))
    Request.httpPost(boxSearch, json.toString())
  } catch {
    case e: requestException =>
      logger.warn(e.getMessage)
      throw connectionException()
    case e: Throwable =>
      logger.error(e.getMessage)
      throw connectionException()
  }

  def getTicketsByWallet(ticketErgoTree: ErgoTree, address: String): Json = try{
    val ergoTreeTemplateHash = scorex.crypto.hash.Sha256(ErgoTreeTemplate.fromErgoTree(ticketErgoTree).getBytes).map("%02x".format(_)).mkString
    val wallet = Address.create(address).getErgoAddress.script.bytes.map("%02x".format(_)).mkString
    val json = Json.fromFields(List(
      ("ergoTreeTemplateHash", Json.fromString(ergoTreeTemplateHash)),
      ("registers", Json.fromFields(List(("R4", Json.fromString(wallet)))))
    ))
    Request.httpPost(boxSearch, json.toString())
  } catch {
    case e: requestException =>
      logger.warn(e.getMessage)
      throw connectionException()
    case e: Throwable =>
      logger.error(e.getMessage)
      throw connectionException()
  }
}
