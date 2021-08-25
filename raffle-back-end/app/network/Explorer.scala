package network

import helpers.Configs
import io.circe.Json
import io.circe.parser.parse
import javax.inject.Singleton
import play.api.libs.json.{Json => playJson}
import scalaj.http.{BaseHttp, HttpConstants}

import scala.util.{Failure, Success, Try}

@Singleton
class Explorer() {
  private val baseUrlV0 = s"${Configs.explorerUrl}/api/v0"
  private val baseUrlV1 = s"${Configs.explorerUrl}/api/v1"
  private val tx = s"$baseUrlV1/transactions"
  private val unconfirmedTx = s"$baseUrlV0/transactions/unconfirmed"
  private val unspentBoxesByTokenId = s"$baseUrlV1/boxes/unspent/byTokenId"
  private val boxesP1 = s"$tx/boxes"
  private val mempoolTransactions = s"$baseUrlV1/mempool/transactions/byAddress/"

  def getTxsInMempoolByAddress(address: String): Json = try {
    GetRequest.httpGet(s"$mempoolTransactions/$address")
  }
  catch {
    case _: Throwable => Json.Null
  }

  def getNumberTxInMempoolByAddress(address: String): Int = try {
    val newJson = getTxsInMempoolByAddress(address)
    val js = playJson.parse(newJson.toString())
    (js \ "total").as[Int]
  }
  catch {
    case _: Throwable => 0
  }

  /**
   * @param txId transaction id
   * @return transaction if it is unconfirmed
   */
  def getUnconfirmedTx(txId: String): Json = try {
    GetRequest.httpGet(s"$unconfirmedTx/$txId")
  }
  catch {
    case _: Throwable => Json.Null
  }

  /**
   * @param txId transaction id
   * @return transaction if it is confirmed (mined)
   */
  def getConfirmedTx(txId: String): Json = try {
    GetRequest.httpGet(s"$tx/$txId")
  }
  catch {
    case _: Throwable => Json.Null
  }

  /**
   * @param txId transaction id
   * @return -1 if tx does not exist, 0 if it is unconfirmed, otherwise, confirmation num
   */
  def getConfNum(txId: String): Int = {
    val unc = getUnconfirmedTx(txId)
    if (unc != Json.Null) 0
    else {
      val conf = getConfirmedTx(txId)
      if (conf != Json.Null) conf.hcursor.downField("summary").as[Json].getOrElse(Json.Null)
        .hcursor.downField("confirmationsCount").as[Int].getOrElse(-1)
      else -1
    }
  }

  def getUnspentTokenBoxes(tokenId: String, offset: Int, limit: Int): Json = try {
    GetRequest.httpGet(s"$unspentBoxesByTokenId/$tokenId?offset=$offset&limit=$limit")
  } catch {
    case _: Throwable => Json.Null
  }

  def getUnspentBoxByID(boxId: String): Json = try {
    GetRequest.httpGet(s"$boxesP1/$boxId")
  } catch {
    case _: Throwable => Json.Null
  }

  def getUnconfirmedTxByAddress(address: String): Json = try {
    GetRequest.httpGet(s"$unconfirmedTx/byAddress/$address")
  } catch {
    case _: Throwable => Json.Null
  }
}

object GetRequest{
  object RaffleHttp extends BaseHttp (None, HttpConstants.defaultOptions, HttpConstants.utf8, 4096, "Mozilla/5.0 (X11; OpenBSD amd64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.81 Safari/537.36",
    true
  )
  private val defaultHeader: Seq[(String, String)] = Seq[(String, String)](("Accept", "application/json"))
  def httpGetWithError(url: String, headers: Seq[(String, String)] = defaultHeader): Either[Throwable, Json] = {
    Try {
      val responseReq = RaffleHttp(url).headers(defaultHeader).asString
      (responseReq.code, responseReq)
    }
    match{
      case Success((200, responseReq)) => parse(responseReq.body)
      case Success((responseHttpCode, responseReq)) => Left(new Exception(s"returned a error with http code $responseHttpCode and error ${responseReq.throwError}"))
      case Failure(exception) => Left(exception)
    }
  }

  def httpGet(url: String): Json = {
    httpGetWithError(url) match {
      case Right(json) => json
      case Left(ex) => throw ex
    }
  }
}
