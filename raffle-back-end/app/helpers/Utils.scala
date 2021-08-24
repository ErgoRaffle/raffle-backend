package helpers

import java.io.{PrintWriter, StringWriter}
import javax.inject.{Inject, Singleton}
import com.typesafe.config.ConfigFactory
import io.circe.Json
import network.{Client, Explorer}
import org.ergoplatform.appkit.{Address, BlockchainContext, ErgoType, ErgoValue, InputBox, JavaHelpers}
import special.collection.Coll

import java.security.MessageDigest
import org.ergoplatform.ErgoAddress
import sigmastate.serialization.ErgoTreeSerializer
import network.GetRequest
import play.api.Logger

import scala.util.matching.Regex
import scala.util.Try

@Singleton
class Utils @Inject()(client: Client, explorer: Explorer) {
  private val logger: Logger = Logger(this.getClass)

  def getStackTraceStr(e: Throwable): String = {
    val sw = new StringWriter
    val pw = new PrintWriter(sw)
    e.printStackTrace(pw)
    sw.toString
  }

  def getAPILink(APIName: String): String = {
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

  def getAddress(addressBytes: Array[Byte]): ErgoAddress = {
    val ergoTree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(addressBytes)
    Configs.addressEncoder.fromProposition(ergoTree).get
  }

  def longListToErgoValue(elements: Array[Long]): ErgoValue[Coll[Long]] = {
    val longColl = JavaHelpers.SigmaDsl.Colls.fromArray(elements)
    ErgoValue.of(longColl, ErgoType.longType())
  }

  def getRaffleBox(tokenId: String):InputBox = {
    client.getClient.execute((ctx: BlockchainContext) => {
      var raffleBoxId: String = ""
      var C: Int = 0
      while (raffleBoxId == "") {
        Try {
          val raffleBoxJson = explorer.getUnspentTokenBoxes(Configs.token.service, C, 100)
          raffleBoxId = raffleBoxJson.hcursor.downField("items").as[List[Json]].getOrElse(null)
            .filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null).size > 1)
            .filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null)(1)
              .hcursor.downField("tokenId").as[String].getOrElse("") == tokenId).head
            .hcursor.downField("boxId").as[String].getOrElse("")
        }
        C += 100
      }

      var raffleBox = ctx.getBoxesById(raffleBoxId).head
      val raffleAddress = Configs.addressEncoder.fromProposition(raffleBox.getErgoTree).get.toString
      val mempool = explorer.getUnconfirmedTxByAddress(raffleAddress)
      try {
        val txs = mempool.hcursor.downField("items").as[List[Json]].getOrElse(throw new Throwable("bad request"))
        var txMap: Map[String, Json] = Map()
        txs.foreach(txJson => {
          val txRaffleInput = txJson.hcursor.downField("inputs").as[List[Json]].getOrElse(throw new Throwable("bad response from explorer")).head
          val id = txRaffleInput.hcursor.downField("id").as[String].getOrElse("")
          txMap += (id -> txJson)
        })
        val keys = txMap.keys.toSeq
        logger.info(raffleBox.getId.toString)
        logger.info(keys.toString())
        logger.info(keys.contains(raffleBox.getId.toString).toString)
        while (keys.contains(raffleBox.getId.toString)) {
          val txJson = txMap(raffleBox.getId.toString).toString()
          var newJson = txJson.replaceAll("id", "boxId")
            .replaceAll("txId", "transactionId")
            .replaceAll("null", "\"\"")
          newJson = newJson.substring(0, 5) + "id" + newJson.substring(10)
          val tmpTx = ctx.signedTxFromJson(newJson)
          raffleBox = tmpTx.getOutputsToSpend.get(0)
        }
      } catch {
        case e: Throwable => logger.error(e.toString)
      }
      raffleBox
    })
  }

  def getServiceBox(): InputBox = {
    client.getClient.execute((ctx: BlockchainContext) => {
      val serviceBoxJson = explorer.getUnspentTokenBoxes(Configs.token.nft, 0, 10)
      val serviceBoxId = serviceBoxJson.hcursor.downField("items").as[List[Json]].getOrElse(throw new Throwable("bad request")).head.hcursor.downField("boxId").as[String].getOrElse("")
      var serviceBox = ctx.getBoxesById(serviceBoxId).head
      val serviceAddress = Configs.addressEncoder.fromProposition(serviceBox.getErgoTree).get.toString

      val mempool = explorer.getUnconfirmedTxByAddress(serviceAddress)
      try {
        val txs = mempool.hcursor.downField("items").as[List[Json]].getOrElse(throw new Throwable("bad request"))
        var txMap: Map[String, Json] = Map()
        txs.foreach(txJson => {
          val txServiceInput = txJson.hcursor.downField("inputs").as[List[Json]].getOrElse(throw new Throwable("bad response from explorer")).head
          val id = txServiceInput.hcursor.downField("id").as[String].getOrElse("")
          txMap += (id -> txJson)
        })
        val keys = txMap.keys.toSeq
        logger.info(serviceBox.getId.toString)
        logger.info(keys.toString())
        logger.info(keys.contains(serviceBox.getId.toString).toString)
        while (keys.contains(serviceBox.getId.toString)) {
          val txJson = txMap(serviceBox.getId.toString).toString()
          var newJson = txJson.toString().replaceAll("id", "boxId")
            .replaceAll("txId", "transactionId")
            .replaceAll("null", "\"\"")
          newJson = newJson.substring(0, 5) + "id" + newJson.substring(10)
          val tmpTx = ctx.signedTxFromJson(newJson)
          serviceBox = tmpTx.getOutputsToSpend.get(0)
        }
      } catch {
        case e: Throwable => logger.debug(e.toString)
      }
      serviceBox
    })
  }

  def checkTransaction(txId: String): Int = {
    if(txId != "") {
      val unconfirmedTx = explorer.getUnconfirmedTx(txId)
      if (unconfirmedTx == Json.Null) {
        val confirmedTx = explorer.getConfirmedTx(txId)
        if (confirmedTx == Json.Null) {
          0 // resend transaction
        } else {
          1 // transaction mined
        }
      } else {
        2 // transaction already in mempool
      }
    }else{
      0
    }
  }

  def isBoxInMemPool(box: InputBox, inputIndex: Int): Boolean = isBoxInMemPool(box, Seq(inputIndex))

  def isBoxInMemPool(box: InputBox, inputIndexes: Seq[Int]) : Boolean = {
    val address = getAddress(box.getErgoTree.bytes)
    val transactions = explorer.getAddressMempoolTransactions(address.toString)
    if(transactions != Json.Null) {
      explorer.getAddressMempoolTransactions(address.toString).hcursor.downField("items").as[List[Json]].getOrElse(throw new Throwable("bad request")).exists(transaction => {
        inputIndexes.exists(inputIndex => {
          transaction.hcursor.downField("inputs").as[List[Json]].getOrElse(throw new Throwable("bad request")).toArray.apply(inputIndex).hcursor.downField("boxId").as[String].getOrElse(throw new Throwable("bad request")) == box.getId.toString
        })
      })
    }else{
      false
    }
  }

  final case class InvalidRecaptchaException(private val message: String = "Invalid recaptcha") extends Throwable(message)

  def verifyRecaptcha(response: String): Unit = {
    try{
      val res = GetRequest.httpGet(s"https://www.google.com/recaptcha/api/siteverify?secret=${Configs.recaptchaKey}&response=${response}")

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

}
