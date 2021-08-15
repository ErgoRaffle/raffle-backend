package controllers

import helpers.{Configs, Utils}
import io.circe.Json
import io.circe.generic.codec.DerivedAsObjectCodec.deriveCodec
import network.{Client, Explorer}
import org.ergoplatform.appkit.{BlockchainContext, ErgoValue}
import play.api.Logger
import play.api.libs.circe.Circe
import play.api.mvc._
import raffle.{CreateReqUtils, DonateReqUtils, RaffleContract, RaffleUtils}
import sigmastate.serialization.ErgoTreeSerializer
import special.collection.Coll

import java.nio.charset.StandardCharsets
import javax.inject._
import scala.util.Try


@Singleton
class HomeController @Inject()(assets: Assets, raffleUtils: RaffleUtils, explorer: Explorer, donateReqUtils: DonateReqUtils,
                               client: Client, raffleContract: RaffleContract, createReqUtils: CreateReqUtils,
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
    try {
      val boxes = explorer.getUnspentTokenBoxes(Configs.token.service, offset, limit)
      val items = boxes.hcursor.downField("items").as[Seq[Json]].getOrElse(throw new Throwable("parse error"))
        .filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null).head
          .hcursor.downField("tokenId").as[String].getOrElse("")
          == Configs.token.service)

      var raffles = ""
      for (i <- items.indices) {
        Try {
          val id = items(i).hcursor.downField("assets").as[Seq[Json]].getOrElse(null)(1)
            .hcursor.downField("tokenId").as[String].getOrElse("")
          val registers = items(i).hcursor.downField("additionalRegisters").as[Json].getOrElse(null)
          val R4: Array[Long] = ErgoValue.fromHex(registers.hcursor.downField("R4").as[Json].getOrElse(null)
            .hcursor.downField("serializedValue").as[String].getOrElse(""))
            .getValue.asInstanceOf[Coll[Long]].toArray
          val charityPercent = R4(0)
          val serviceFee = R4(1)
          val winnerPercent = 100 - charityPercent - serviceFee
          val ticketPrice = R4(2)
          val goal = R4(3)
          val deadlineHeight = R4(4)

          val strListByte : Array[Coll[Byte]] = ErgoValue.fromHex(registers.hcursor.downField("R6").as[Json].getOrElse(null)
            .hcursor.downField("serializedValue").as[String].getOrElse(""))
            .getValue.asInstanceOf[Coll[Coll[Byte]]].toArray
          val name: String = new String(strListByte(0).toArray, StandardCharsets.UTF_8)
          val description: String = new String(strListByte(1).toArray, StandardCharsets.UTF_8)

          raffles = raffles +
          s"""{
             | "id" : "$id",
             | "name" : "$name",
             | "description" : "$description",
             | "deadline" : $deadlineHeight,
             | "winnerPercent" : $winnerPercent,
             | "charityPercent" : $charityPercent,
             | "minToRaise" : $goal,
             }""".stripMargin
          if (i != items.size - 1)
          raffles = raffles + ", "
        }
      }
    val total = items.size
    Ok(
        s"""{
            |  "items": [
            |    $raffles
            |  ],
            |  "total": $total
            |}""".stripMargin
        ).as("application/json")
    } catch {
      case e: Throwable => exception(e)
    }
  }


  def getRafflesByTokenId(tokenId: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      logger.info("Responding get raffle request by token id: "+ tokenId)

      // TODO : raffle address must be replaced
      val raffleAdd = "2qheKziy2VUtAjCVNMuNgbPrqcEXXtd5FRKuYwU5Z4WtZBmAtFPLb8yq21a4AQBNRcSQwQ5ryz8Am4WWTDuJfAWnDmE6J5h1ruSDkizyAqSnyByNW8PzPy1Y7xCmq2PBg4cZzJqnQmbbZq6x7J5hNoQv6i96CXk3YMVJMYdzqNQq8hBSicBTEAbbpfgqQJMrtTaAAQSHWoxbBp5R75bAwvoje4qQZGKRTpJbChukxYLBQDsmrfNTDeSxqJUPFZHZ6ZBfT27ia7qToBnmAUaKCzJ5AqniBWvmmcmi2ramGZik9zQd1wtrJSpyb4CZy2JPHnXJp6fG3RwEQjJvZbgcgVb9VVYvF9imc1rrJ3BdKu7jrwXP84dBAjQuqqpKJts4Ay8ncMw2pBabh52r9gtUFhDRbyQYPacBUsbBqNosJAiaXvQqjUxUFA2qzfkW5E6UWp5XDLtPtxANYHC9o8oVduapauSoCK3gEvqJnptn11JaRwzwjcjB4agK1Xj5xmDvAeq9TcEXnwMAELfso5y6EDWgHJRnEjbMMfAMQRmS2cJYeCJQKnymsuivWJob1xuB1PcNkHCXJHn3zCetJiCnrcKyzZ3y7DTV8aX2Mmr5NnphQdxuPxawhqEGGSfspCPMU2qGDQp9A4gmvN96WAqk98gzihYwwkgEM4xAzcXrhxbfF3RYcu7b7uscto96hqMrVQ258UyRdqUpQUcrYWeYQpoD78Zwesytx9oWK8gJL6Q5GFrHD8Zx2T6LtLh12oeHbxhe2hjnAd2WBtS82k5jpnXZNSyrqE22uwoDzS5kruw11ZdB8gjTrohPmyHJvWCZTXfdw1N8J2nghD67NJJy3JJikvY1JpncQ8somidi6gdLKene8KCvoimLTPgWXK1oK3m34HjsfHayZbrNhmviWpgGDL7gY1FJrVe9BjP6LfMe8QVfd82yFsye8YE979H8mCuPjqR4enG4iN61SnhjevkMkyNhTQu6XkdjgLrsHhW341um3FEHs3Ay2C7njfK92vBDPiXoWBmeb5poGKd74Ue7zD3UtXCQuiesNmXR2KtDfeYY6R1nd2J36J8RX6DJqd39ajy29VNt7FALQvZ21bWFAAb4ExVQTGjX7DkqeLXQgK84yQPC9yeY4DFeLok4HujVry81YXEaat9wHqMga3BNigzLAD3p8QZobNdRSB6Cd5iY6hhYsvnhmcGdrEAYnLn1mZ4bW4RpVUHq5HyXdzHSJC7Ap9ZSvxqgvhHor6KAFBdymm1CXpXXQ2uYkgziB3BzYJBQk7Ngmoc2qQV9WrmAJRZCSNPd7kqNm7rveZn9DBM6LbjT7xFPxKUGAegkwHbeWNM1P5QSGPJUtW1ciGm9p84hR5jNcWbKJyJYjjrZ7Z"
      var boxes = explorer.getUnspentTokenBoxes(Configs.token.service, 0, 100)
      var items = boxes.hcursor.downField("items").as[Seq[Json]].getOrElse(throw new Throwable("parse error"))
        .filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null).head
          .hcursor.downField("tokenId").as[String].getOrElse("") == Configs.token.service)
        .filter(_.hcursor.downField("address").as[String].getOrElse("") == raffleAdd)

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
      val fee = Configs.fee

      val raffles = s"""{
           | "id" : "$id",
           | "name" : "$name",
           | "description" : "$description",
           | "deadline" : $deadlineHeight,
           | "erg" : $totalRaised,
           | "totalSoldTickets" : $totalSoldTicket,
           | "charityAddr" : "$charityAddress",
           | "winnerPercent" : $winnerPercent,
           | "charityPercent" : $charityPercent,
           | "min" : $goal,
           | "ticketPrice" : $ticketPrice,
           | "fee" : $fee
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
      client.getClient.execute((ctx: BlockchainContext) => {
        val name: String = request.body.hcursor.downField("name").as[String].getOrElse(throw new Throwable("name field must exist"))
        val description: String = request.body.hcursor.downField("description").as[String].getOrElse(throw new Throwable("description field must exist"))
        val goal: Long = request.body.hcursor.downField("goal").as[Long].getOrElse(throw new Throwable("minToRaise field must exist"))
        val deadlineHeight: Long = request.body.hcursor.downField("deadlineHeight").as[Long].getOrElse(throw new Throwable("deadlineHeight field must exist"))
        val charityPercent: Int = request.body.hcursor.downField("charityPercent").as[Int].getOrElse(throw new Throwable("charityPercent field must exist"))
        val charityAddr: String = request.body.hcursor.downField("charityAddr").as[String].getOrElse(throw new Throwable("charityAddr field must exist"))
        val walletAddr: String = request.body.hcursor.downField("walletAddr").as[String].getOrElse(throw new Throwable("charityAddr field must exist"))
        val ticketPrice: Long = request.body.hcursor.downField("ticketPrice").as[Long].getOrElse(throw new Throwable("charityAddr field must exist"))
        val paymentAddress = createReqUtils.CreateRaffleProxyAddress(walletAddr, charityPercent, name, description, deadlineHeight + ctx.getHeight, charityAddr, goal, ticketPrice)
        val amount = Configs.fee * 4
        val delay = Configs.creationDelay

        Ok(
          s"""{
             |  "deadline": $delay,
             |  "erg": $amount,
             |  "address": "$paymentAddress"
             |}""".stripMargin
        ).as("application/json")
      })
    } catch {
      case e: Throwable => exception(e)
    }
  }

  def createService(): Action[AnyContent] = Action {implicit request: Request[AnyContent] =>
    try {
      val boxId = raffleUtils.createService()
      Ok(
        s"""{
           |  txId : $boxId
           |}""".stripMargin
      ).as("application/json")
    } catch {
      case e: Throwable => exception(e)
    }
  }

  // Wallet + Ticket Count + Raffle Token ID
  def donateToId: Action[Json] = Action(circe.json) { implicit request =>
    val raffleId: String = request.body.hcursor.downField("id").as[String].getOrElse(throw new Throwable("id field must exist"))
    val walletAddr: String = request.body.hcursor.downField("walletAddr").as[String].getOrElse(throw new Throwable("walletAddr field must exist"))
    val ticketCounts: Long = request.body.hcursor.downField("ticketCounts").as[Long].getOrElse(throw new Throwable("erg field must exist"))
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
  }

  def servicePercent(): Action[AnyContent] = Action {
    val p = Configs.servicePercent
    Ok(
      s"""{
        | "z" : $p
        |}""".stripMargin).as("application/json")
  }
}
