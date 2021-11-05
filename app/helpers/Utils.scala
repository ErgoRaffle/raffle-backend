package helpers

import java.io.{PrintWriter, StringWriter}

import javax.inject.{Inject, Singleton}
import io.circe.{Json => ciJson}
import network.{Client, Explorer}
import org.ergoplatform.appkit.{BlockchainContext, ErgoClientException, ErgoType, ErgoValue, InputBox, JavaHelpers}
import special.collection.Coll
import java.util.Calendar

import org.ergoplatform.ErgoAddress
import sigmastate.serialization.ErgoTreeSerializer
import network.Request
import play.api.Logger
import play.api.libs.json._
import raffle.Addresses

import scala.collection.mutable.Seq
import scala.util.Try

final case class InvalidRecaptchaException(private val message: String = "Invalid recaptcha") extends Throwable(message)
final case class paymentNotCoveredException(private val message: String = "Payment not Covered") extends Throwable(message)
final case class failedTxException(private val message: String = "Tx sending failed") extends Throwable(message)
final case class explorerException(private val message: String = "Explorer error") extends Throwable(message)
final case class connectionException(private val message: String = "Network Error") extends Throwable(message)
final case class parseException(private val message: String = "Parsing failed") extends Throwable(message)
final case class finishedRaffleException(private val message: String = "raffle finished") extends Throwable(message)
final case class skipException(private val message: String = "skip") extends Throwable(message)
final case class proveException(private val message: String = "Tx proving failed") extends Throwable(message)
final case class internalException(private val message: String = "something went wrong") extends Throwable(message)
final case class noRaffleException(private val message: String = "No raffle found in the network") extends Throwable(message)


@Singleton
class Utils @Inject()(client: Client, explorer: Explorer, addresses: Addresses) {
  private val logger: Logger = Logger(this.getClass)

  def getStackTraceStr(e: Throwable): String = {
    val sw = new StringWriter
    val pw = new PrintWriter(sw)
    e.printStackTrace(pw)
    sw.toString
  }


  def getAddress(addressBytes: Array[Byte]): ErgoAddress = {
    val ergoTree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(addressBytes)
    Configs.addressEncoder.fromProposition(ergoTree).get
  }

  def longListToErgoValue(elements: Array[Long]): ErgoValue[Coll[Long]] = {
    val longColl = JavaHelpers.SigmaDsl.Colls.fromArray(elements)
    ErgoValue.of(longColl, ErgoType.longType())
  }


  def findMempoolBox(address: String, box: InputBox, ctx: BlockchainContext): InputBox = {
    try {
      val mempool = Json.parse(explorer.getUnconfirmedTxByAddress(address).toString())
      var outBox = box
      val txs = (mempool \ "items").as[List[JsValue]]
      var txMap: Map[String, JsValue] = Map()
      txs.foreach(txJson => {
        val txRaffleInput = (txJson \ "inputs").as[List[JsValue]].head
        val id = (txRaffleInput \ "id").as[String]
        txMap += (id -> txJson)
      })
      val keys = txMap.keys.toSeq
      logger.debug(outBox.getId.toString)
      logger.debug(keys.toString())
      while (keys.contains(outBox.getId.toString)) {
        val txJson = txMap(outBox.getId.toString)
        val inputs = (txJson \ "inputs").as[JsValue].toString().replaceAll("id", "boxId")
        val outputs = (txJson \ "outputs").as[JsValue].toString().replaceAll("id", "boxId").replaceAll("txId", "transactionId")
        val dataInputs = (txJson\ "dataInputs").as[JsValue].toString()
        val id = (txJson \ "id").as[String]
        val newJson = s"""{
          "id" : "${id}",
          "inputs" : ${inputs},
          "dataInputs" : ${dataInputs},
          "outputs" : ${outputs}
          }"""
        val tmpTx = ctx.signedTxFromJson(newJson.replaceAll("null", "\"\""))
        outBox = tmpTx.getOutputsToSpend.get(0)
      }
      outBox
    } catch {
      case e: explorerException => {
        logger.warn(e.getMessage)
        throw connectionException()
      }
      case _: parseException => throw connectionException()
      case e: Throwable => {
        logger.error(getStackTraceStr(e))
        throw new Throwable("Something is wrong")
      }
    }
  }

  def getRaffleBox(tokenId: String): InputBox = {
    try {
      client.getClient.execute((ctx: BlockchainContext) => {
        var raffleBoxId: String = ""
        var C: Int = 0
        while (raffleBoxId == "") {
          // TODO edit here
          Try {
            val raffleBoxciJson = explorer.getUnspentTokenBoxes(Configs.token.service, C, 100)
            raffleBoxId = raffleBoxciJson.hcursor.downField("items").as[List[ciJson]].getOrElse(null)
              .filter(_.hcursor.downField("assets").as[Seq[ciJson]].getOrElse(null).size > 1)
              .filter(_.hcursor.downField("assets").as[Seq[ciJson]].getOrElse(null)(1)
                .hcursor.downField("tokenId").as[String].getOrElse("") == tokenId).head
              .hcursor.downField("boxId").as[String].getOrElse("")
          }
          C += 100
        }
        var raffleBox = ctx.getBoxesById(raffleBoxId).head
        val raffleAddress = Configs.addressEncoder.fromProposition(raffleBox.getErgoTree).get.toString
        Try {
          raffleBox = findMempoolBox(raffleAddress, raffleBox, ctx)
        }
        raffleBox
      })
    } catch {
      case e: ErgoClientException =>{
        logger.warn(e.getMessage)
        throw connectionException()
      }
      case _: connectionException => throw connectionException()
      case e: Throwable => {
        logger.error(getStackTraceStr(e))
        throw new Throwable("Something is wrong")
      }
    }
  }

  def getServiceBox(): InputBox = {
    try {
      client.getClient.execute((ctx: BlockchainContext) => {
        val serviceBoxciJson = explorer.getUnspentTokenBoxes(Configs.token.nft, 0, 100)
        val serviceBoxId = serviceBoxciJson.hcursor.downField("items").as[List[ciJson]].getOrElse(throw parseException())
          .head.hcursor.downField("boxId").as[String].getOrElse("")
        var serviceBox = ctx.getBoxesById(serviceBoxId).head
        val serviceAddress = Configs.addressEncoder.fromProposition(serviceBox.getErgoTree).get.toString
        Try {
          serviceBox = findMempoolBox(serviceAddress, serviceBox, ctx)
        }
        serviceBox
      })
    } catch {
      case _: connectionException => throw connectionException()
      case e: ErgoClientException =>{
        logger.warn(e.getMessage)
        throw connectionException()
      }
      case e: explorerException => {
        logger.warn(e.getMessage)
        throw connectionException()
      }
      case _: parseException => throw connectionException()
      case e: Throwable => {
        logger.error(getStackTraceStr(e))
        throw new Throwable("Something is wrong")
      }
    }
  }

  def checkTransaction(txId: String): Int = {
    try {
      if (txId != "") {
        val unconfirmedTx = explorer.getUnconfirmedTx(txId)
        if (unconfirmedTx == ciJson.Null) {
          val confirmedTx = explorer.getConfirmedTx(txId)
          if (confirmedTx == ciJson.Null) {
            0 // resend transaction
          } else {
            1 // transaction mined
          }
        } else {
          2 // transaction already in mempool
        }
      } else {
        0
      }
    } catch {
      case e: explorerException => {
        logger.warn(e.getMessage)
        throw connectionException()
      }
      case e: Throwable => {
        logger.error(getStackTraceStr(e))
        throw new Throwable("Something is wrong")
      }
    }
  }

  def isBoxInMemPool(box: InputBox) : Boolean = {
    try {
      val address = getAddress(box.getErgoTree.bytes)
      val transactions = Json.parse(explorer.getTxsInMempoolByAddress(address.toString).toString())
      if (transactions != null) {
        (transactions \ "items").as[List[JsValue]].exists(tx =>{
          if((tx \ "inputs").as[JsValue].toString().contains(box.getId.toString)) true
          else false
        })
      } else {
        false
      }
    } catch {
      case e: explorerException => {
        logger.warn(e.getMessage)
        throw connectionException()
      }
      case e: parseException => {
        logger.warn(e.getMessage)
        throw connectionException()
      }
      case e: Throwable => {
        logger.error(getStackTraceStr(e))
        throw new Throwable("Something is wrong")
      }
    }
  }

  def verifyRecaptcha(response: String): Unit = {
    try{
      val res = Request.httpGet(s"https://www.google.com/recaptcha/api/siteverify?secret=${Configs.recaptchaKey}&response=${response}")

      if (res.hcursor.downField("success").as[Boolean].getOrElse(new Throwable("parse error")) == false) {
        logger.info(s"response of google ${res}")
        throw new InvalidRecaptchaException
      }
    } catch {
      case _: InvalidRecaptchaException => throw new InvalidRecaptchaException
      case _: Throwable => throw new Throwable("problem in verify recaptcha")
    }
  }

  def validateErgValue(value: Long): Unit = {
    if (value < 10000) throw new Throwable("Minimum value is 0.00001 Erg")
  }

  def validateAddress(address: String, name: String): Unit = {
    try {
      Configs.addressEncoder.fromString(address).get.script
    }
    catch {
      case _: Throwable => throw new Throwable(s"Invalid ${name} address")
    }
  }

  def validateCharityPercent(charity: Int, service: Long): Unit = {
    if (charity < 1) throw new Throwable("Charity share should be positive")
    else if (charity + service > 99) throw new Throwable("Sum of charity share and service share should be less than 100")
  }

  def validateDeadline(value: Long): Unit = {
    if (value < 1) throw new Throwable("Deadline should be positive")
    else if (value > 262800) throw new Throwable("Maximum deadline is 262800 blocks (about 1 year)")
  }

  def validateTicketCounts(value: Long): Unit = {
    if (value < 1) throw new Throwable("Ticket counts should be positive")
  }

  def currentTime: Long = Calendar.getInstance().getTimeInMillis / 1000

  def getTransactionFrontLink(txId: String): String = Configs.explorerFront + "/en/transactions/" + txId

}
