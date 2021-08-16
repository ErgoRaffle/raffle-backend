package controllers

import helpers.{Configs, Utils}
import io.circe.Json
import io.circe.generic.codec.DerivedAsObjectCodec.deriveCodec
import network.{Client, Explorer}
import org.ergoplatform.appkit.ErgoValue
import play.api.Logger
import play.api.libs.circe.Circe
import play.api.mvc._
import raffle.{Addresses, CreateReqUtils, DonateReqUtils}
import sigmastate.serialization.ErgoTreeSerializer
import special.collection.Coll

import java.nio.charset.StandardCharsets
import javax.inject._
import net.sf.ehcache.search.aggregator.Count

import scala.collection.mutable.{ListBuffer, Seq}
import scala.util.Try


@Singleton
class HomeController @Inject()(assets: Assets, addresses: Addresses, explorer: Explorer, donateReqUtils: DonateReqUtils,
                               client: Client, createReqUtils: CreateReqUtils,
                               val controllerComponents: ControllerComponents, utils: Utils) extends BaseController
  with Circe {
  private val logger: Logger = Logger(this.getClass)

  def index: Action[AnyContent] = {
    assets.at("index.html")
  }

  def assetOrDefault(resource: String): Action[AnyContent] = {
    if (resource.contains(".")) assets.at(resource) else index
  }

  def exception(e: Throwable): Result = {
    logger.error(utils.getStackTraceStr(e))
    BadRequest(s"""{"success": false, "message": "${e.getMessage}"}""").as("application/json")
  }


  def getRaffles(offset: Int, limit: Int) = Action { implicit request: Request[AnyContent] =>
    logger.info("Responding get raffles request")
    var raffleCount = 0
    var raffles: ListBuffer[Json] = ListBuffer()
    var explorerOffset: Int = 0
    var boxes = explorer.getUnspentTokenBoxes(Configs.token.service, 0, 100)
    val total = boxes.hcursor.downField("total").as[Int].getOrElse(0)
    while(raffleCount < offset+limit && explorerOffset < total) {
      Try {
        val items = boxes.hcursor.downField("items").as[Seq[Json]].getOrElse(throw new Throwable("parse error"))
          .filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null).size > 1)
          .filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null).head
            .hcursor.downField("tokenId").as[String].getOrElse("") == Configs.token.service)

        for (i <- items.indices) {
          raffleCount += 1
          if (raffleCount-1 >= offset && raffleCount <= offset + limit) {
            val id = items(i).hcursor.downField("assets").as[Seq[Json]].getOrElse(null)(1)
              .hcursor.downField("tokenId").as[String].getOrElse("")
            val registers = items(i).hcursor.downField("additionalRegisters").as[Json].getOrElse(null)
            val R4: Array[Long] = ErgoValue.fromHex(registers.hcursor.downField("R4").as[Json].getOrElse(null)
              .hcursor.downField("serializedValue").as[String].getOrElse(""))
              .getValue.asInstanceOf[Coll[Long]].toArray
            val charityPercent = R4(0)
            val serviceFee = R4(1)
            val winnerPercent = 100 - charityPercent - serviceFee
            val goal = R4(3)
            val deadlineHeight = R4(4)

            val strListByte: Array[Coll[Byte]] = ErgoValue.fromHex(registers.hcursor.downField("R6").as[Json].getOrElse(null)
              .hcursor.downField("serializedValue").as[String].getOrElse(""))
              .getValue.asInstanceOf[Coll[Coll[Byte]]].toArray
            val name: String = new String(strListByte(0).toArray, StandardCharsets.UTF_8)
            val description: String = new String(strListByte(1).toArray, StandardCharsets.UTF_8)
            raffles += Json.fromFields(List(
              ("id", Json.fromString(id)),
              ("name", Json.fromString(name)),
              ("description", Json.fromString(description)),
              ("deadline", Json.fromLong(deadlineHeight)),
              ("winnerPercent", Json.fromLong(winnerPercent)),
              ("charityPercent", Json.fromLong(charityPercent)),
              ("minToRaise", Json.fromLong(goal)),
            ))
          }
        }
      }
      explorerOffset += 100
      boxes = explorer.getUnspentTokenBoxes(Configs.token.service, explorerOffset, 100)
    }
    try {
      val currentHeight: Long = client.getHeight
      var totalRaffles = 0
      if(raffleCount > limit+offset) totalRaffles = limit
      else totalRaffles = raffleCount - offset
      val result = Json.fromFields(List(
        ("items", Json.fromValues(raffles.toList)),
        ("total", Json.fromInt(totalRaffles)),
        ("currentHeight", Json.fromLong(currentHeight))
      ))

      Ok(result.toString()).as("application/json")
    } catch {
      case e: Throwable => exception(e)
    }
  }


  def getRafflesByTokenId(tokenId: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      logger.info("Responding get raffle request by token id: "+ tokenId)

      val raffleAdd = Configs.addressEncoder.fromProposition(addresses.getRaffleActiveContract().getErgoTree).get
      var boxes = explorer.getUnspentTokenBoxes(Configs.token.service, 0, 100)
      var items = boxes.hcursor.downField("items").as[Seq[Json]].getOrElse(throw new Throwable("parse error"))
        .filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null).size > 1)
        .filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null).head
          .hcursor.downField("tokenId").as[String].getOrElse("") == Configs.token.service)
        .filter(_.hcursor.downField("address").as[String].getOrElse("") == raffleAdd.toString)

      var c: Int = 1
      var item : Json = items.filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null)(1)
        .hcursor.downField("tokenId").as[String].getOrElse("") == tokenId).head

      while (item == null) {
        boxes = explorer.getUnspentTokenBoxes(Configs.token.service, c*100, 100)
        items = boxes.hcursor.downField("items").as[Seq[Json]].getOrElse(throw new Throwable("parse error"))
          .filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null).head
            .hcursor.downField("tokenId").as[String].getOrElse("")
            == Configs.token.service)
        c += 1
        item = items.filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null).head
          .hcursor.downField("tokenId").as[String].getOrElse("") == tokenId).head
      }

      val id = item.hcursor.downField("assets").as[Seq[Json]].getOrElse(null)(1)
        .hcursor.downField("tokenId").as[String].getOrElse("")
      val registers = item.hcursor.downField("additionalRegisters").as[Json].getOrElse(null)
      val R4: Array[Long] = ErgoValue.fromHex(registers.hcursor.downField("R4").as[Json].getOrElse(null)
        .hcursor.downField("serializedValue").as[String].getOrElse(""))
        .getValue.asInstanceOf[Coll[Long]].toArray
      val charityPercent = R4(0)
      val serviceFee = R4(1)
      val winnerPercent = 100 - charityPercent - serviceFee
      val ticketPrice = R4(2)
      val goal = R4(3)
      val deadlineHeight = R4(4)
      val totalSoldTicket = R4(5)
      val totalRaised = totalSoldTicket * ticketPrice

      val strListByte : Array[Coll[Byte]] = ErgoValue.fromHex(registers.hcursor.downField("R6").as[Json].getOrElse(null)
        .hcursor.downField("serializedValue").as[String].getOrElse(""))
        .getValue.asInstanceOf[Coll[Coll[Byte]]].toArray
      val name: String = new String(strListByte(0).toArray, StandardCharsets.UTF_8)
      val description: String = new String(strListByte(1).toArray, StandardCharsets.UTF_8)

      val charityAddressByte: Array[Byte] = ErgoValue.fromHex(registers.hcursor.downField("R5").as[Json].getOrElse(null)
        .hcursor.downField("serializedValue").as[String].getOrElse(""))
        .getValue.asInstanceOf[Coll[Byte]].toArray
      val charityAddress = Configs.addressEncoder.fromProposition(ErgoTreeSerializer.DefaultSerializer
        .deserializeErgoTree(charityAddressByte)).get.toString
      val currentHeight: Long = client.getHeight
      val fee = Configs.fee

      val raffles = s"""{
           | "id" : "$id",
           | "name" : "$name",
           | "description" : "$description",
           | "deadline" : $deadlineHeight,
           | "erg" : $totalRaised,
           | "charityAddr" : "$charityAddress",
           | "winnerPercent" : $winnerPercent,
           | "charityPercent" : $charityPercent,
           | "min" : $goal,
           | "ticketPrice" : $ticketPrice,
           | "fee" : $fee,
           | "currentHeight" : $currentHeight
            }""".stripMargin
      Ok(raffles).as("application/json")
    }
    catch {
      case e: Throwable => exception(e)
    }
  }

  /*
  {
    winnerPercent :
    deadlineHeight :
    minToRaise :
    name : ""
    description : ""
    charityAddr : ""
  }
   */
  def addRaffle(): Action[Json] = Action(circe.json) { implicit request =>
    try {
      val name: String = request.body.hcursor.downField("name").as[String].getOrElse(throw new Throwable("name field must exist"))
      val description: String = request.body.hcursor.downField("description").as[String].getOrElse(throw new Throwable("description field must exist"))
      val goal: Long = request.body.hcursor.downField("goal").as[Long].getOrElse(throw new Throwable("minToRaise field must exist"))
      val deadlineHeight: Long = request.body.hcursor.downField("deadlineHeight").as[Long].getOrElse(throw new Throwable("deadlineHeight field must exist"))
      val charityPercent: Int = request.body.hcursor.downField("charityPercent").as[Int].getOrElse(throw new Throwable("charityPercent field must exist"))
      val charityAddr: String = request.body.hcursor.downField("charityAddr").as[String].getOrElse(throw new Throwable("charityAddr field must exist"))
      val walletAddr: String = request.body.hcursor.downField("walletAddr").as[String].getOrElse(throw new Throwable("charityAddr field must exist"))
      val ticketPrice: Long = request.body.hcursor.downField("ticketPrice").as[Long].getOrElse(throw new Throwable("charityAddr field must exist"))
      val captcha: String = request.body.hcursor.downField("captcha").as[String].getOrElse("")
      utils.verifyRecaptcha(captcha)
      val paymentAddress = createReqUtils.CreateRaffleProxyAddress(walletAddr, charityPercent, name, description, deadlineHeight + client.getHeight, charityAddr, goal, ticketPrice)
      val amount = Configs.fee * 4
      val delay = Configs.creationDelay

      Ok(
        s"""{
           |  "deadline": $delay,
           |  "fee": $amount,
           |  "address": "$paymentAddress"
           |}""".stripMargin
      ).as("application/json")
    } catch {
      case e: Throwable => exception(e)
    }
  }


  def donateToId: Action[Json] = Action(circe.json) { implicit request =>
    val raffleId: String = request.body.hcursor.downField("id").as[String].getOrElse(throw new Throwable("id field must exist"))
    val walletAddr: String = request.body.hcursor.downField("walletAddr").as[String].getOrElse(throw new Throwable("walletAddr field must exist"))
    val ticketCounts: Long = request.body.hcursor.downField("ticketCounts").as[Long].getOrElse(throw new Throwable("erg field must exist"))
    val captcha: String = request.body.hcursor.downField("captcha").as[String].getOrElse("")
    try {
      utils.verifyRecaptcha(captcha)
      val response = donateReqUtils.findProxyAddress(walletAddr, raffleId, ticketCounts)
      val paymentAddress = response._1
      val fee = response._2

      Ok(
        s"""{
           | "deadline": 900,
           | "address": "$paymentAddress",
           | "fee" : $fee
           |}""".stripMargin
      ).as("application/json")
    } catch {
      case _: Throwable => throw new Throwable("problem in verifying recaptcha")
    }
  }

  def servicePercent(): Action[AnyContent] = Action {
//    val p = Configs.servicePercent
    val serviceBox = utils.getServiceBox()
    val p = serviceBox.getRegisters.get(0).getValue.asInstanceOf[Long]
    Ok(
      s"""{
        | "z" : $p
        |}""".stripMargin).as("application/json")
  }
}
