package helpers

import java.io.{PrintWriter, StringWriter}

import javax.inject.{Inject, Singleton}
import com.typesafe.config.ConfigFactory
import io.circe.Json
import network.{Client, Explorer}
import org.ergoplatform.ErgoAddress
import org.ergoplatform.appkit.{Address, BlockchainContext, ErgoType, ErgoValue, InputBox, JavaHelpers}
import sigmastate.serialization.ErgoTreeSerializer
import special.collection.Coll
import java.security.MessageDigest

import org.ergoplatform.ErgoAddress
import sigmastate.serialization.ErgoTreeSerializer
import network.GetRequest
import play.api.Logger

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

  def getAddress(addressBytes: Array[Byte]): ErgoAddress = {
    val ergoTree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(addressBytes)
    Configs.addressEncoder.fromProposition(ergoTree).get
  }

  def longListToErgoValue(elements: Array[Long]): ErgoValue[Coll[Long]] = {
    val longColl = JavaHelpers.SigmaDsl.Colls.fromArray(elements)
    ErgoValue.of(longColl, ErgoType.longType())
  }

  def getServiceBox(): InputBox = {
    client.getClient.execute((ctx: BlockchainContext) => {
      val serviceBoxJson = explorer.getUnspentTokenBoxes(Configs.token.nft, 0, 10)
      val serviceBoxId = serviceBoxJson.hcursor.downField("items").as[List[Json]].getOrElse(throw new Throwable("bad request")).head.hcursor.downField("boxId").as[String].getOrElse("")
      var serviceBox = ctx.getBoxesById(serviceBoxId).head
      val serviceAddress = Configs.addressEncoder.fromProposition(serviceBox.getErgoTree).get.toString
//      val mempool = explorer.getAddressMempoolTransactions(serviceAddress)
//      try {
//        val txs = mempool.hcursor.downField("items").as[List[Json]].getOrElse(throw new Throwable("bad request"))
//        var txMap: Map[String, Json] = Map()
//        txs.foreach(txJson => {
//          val txServiceInput = txJson.hcursor.downField("inputs").as[List[Json]].getOrElse(throw new Throwable("bad response from explorer")).head
//          val id = txServiceInput.hcursor.downField("boxId").as[String].getOrElse("")
//          txMap += (id -> txJson)
//        })
//        val keys = txMap.keys.toSeq
//        while (keys.contains(serviceBox.getId.toString)) {
//          val tmpTx = ctx.signedTxFromJson(txMap(serviceBox.getId.toString).toString())
//          serviceBox = tmpTx.getOutputsToSpend.get(0)
//        }
//      } catch {
//        case e: Throwable => println(e)
//      }
      serviceBox
    })
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

}
