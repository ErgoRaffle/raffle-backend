package controllers

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.TypedActor.dispatcher

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import helpers.{Configs, Utils}
import scala.reflect.ClassTag
import scala.util.{Try, Success, Failure}
import io.circe.Json
import io.circe.generic.codec.DerivedAsObjectCodec.deriveCodec
import network.{Client, Explorer}
import play.api.libs.circe.Circe
import play.api.mvc._
import raffle.Adaptor

import javax.inject._
import scala.util.control.Breaks.{break, breakable}


@Singleton
class HomeController @Inject()(assets: Assets, adaptor: Adaptor, explorer: Explorer, client: Client, val controllerComponents: ControllerComponents, utils: Utils) extends BaseController
  with Circe {

  def index: Action[AnyContent] = {
    assets.at("index.html")
  }

  def assetOrDefault(resource: String): Action[AnyContent] = {
    if (resource.contains(".")) assets.at(resource) else index
  }

  def exception(e: Throwable): Result = {
    BadRequest(s"""{"success": false, "message": "${e.getMessage}"}""").as("application/json")
  }




  def getRaffles(offset: Int, limit: Int) = Action { implicit request: Request[AnyContent] =>
    try {
      val boxes = explorer.getUnspentTokenBoxes(Configs.serviceTokenId, offset, limit)
      println(boxes.toString())
      adaptor.updateServiceBox()
      val items = boxes.hcursor.downField("items").as[Array[Json]].getOrElse(throw new Throwable("parse error"))
        .filter(_.hcursor.downField("boxId").as[String].getOrElse(throw new Throwable("BoxId Not Found"))
          != adaptor.serviceBox.getId.toString)
      val tokens = items.map(m => m.hcursor.downField("assets").as[Array[Json]].getOrElse(throw new Throwable("parse error"))(0)
        .hcursor.downField("tokenId").as[String].getOrElse(throw new Throwable("parse error")))
      val result = items.map(m => adaptor.boxRegister(m.hcursor.downField("boxId").as[String]
        .getOrElse(throw new Throwable("BoxId Not Found"))))
      var raffles = ""
      for (i <- 0 to result.size - 1) {
        Try {
          val parsed = io.circe.jawn.parse(result(i))
            .getOrElse(throw new Throwable("parse error"))
          val id = tokens(i).toString
          println("result i: " + result(i))
          val name = parsed.hcursor.downField("name").as[String].getOrElse(throw new Throwable("Name not Found"))
          val description = parsed.hcursor.downField("description").as[String].getOrElse(throw new Throwable("description not Found"))
          val deadline = parsed.hcursor.downField("deadlineHeight").as[Long].getOrElse(throw new Throwable("deadline not Found"))
          val winnerPercent = parsed.hcursor.downField("winnerPercent").as[Int].getOrElse(throw new Throwable("winnerPercent not Found"))
          val charityPercent = parsed.hcursor.downField("charityPercent").as[Int].getOrElse(throw new Throwable("charityPercent not Found"))
          val minToRaise = parsed.hcursor.downField("minToRaise").as[Long].getOrElse(throw new Throwable("minToRaise not Found"))
          raffles = raffles +
          s"""{
             | "id" : "$id",
             | "name" : "$name",
             | "deadline" : $deadline,
             | "winnerPercent" : $winnerPercent,
             | "charityPercent" : $charityPercent,
             | "minToRaise" : $minToRaise
             }""".stripMargin
          if (i != result.size - 1)
          raffles = raffles + ", "
        }
      }
    val total = result.size
    Ok(
        s"""{
            |  "code": 200,
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

  def getRafflesById(raffleId: String) = Action { implicit request: Request[AnyContent] =>
    try {
      Ok(
        adaptor.boxRegister(raffleId)
      ).as("application/json")
    } catch {
      case e: Throwable => exception(e)
    }
  }



  def getRafflesByTokenId(tokenId: String) = Action { implicit request: Request[AnyContent] =>
    try {
      val box = explorer.getUnspentTokenBoxes(tokenId, 0, 100).hcursor.downField("items").as[Array[Json]]
        .getOrElse(throw new Throwable("parse error"))(0)
      val parsed = io.circe.jawn.parse(adaptor.boxRegister(box.hcursor.downField("boxId").as[String]
        .getOrElse(throw new Throwable("BoxId Not Found"))))
        .getOrElse(throw new Throwable("parse error"))
      val id = tokenId
      val name = parsed.hcursor.downField("name").as[String].getOrElse(throw new Throwable("Name not Found"))
      val description = parsed.hcursor.downField("description").as[String].getOrElse(throw new Throwable("description not Found"))
      val deadline = parsed.hcursor.downField("deadlineHeight").as[Long].getOrElse(throw new Throwable("deadline not Found"))
      val winnerPercent = parsed.hcursor.downField("winnerPercent").as[Int].getOrElse(throw new Throwable("winnerPercent not Found"))
      val charityPercent = parsed.hcursor.downField("charityPercent").as[Int].getOrElse(throw new Throwable("charityPercent not Found"))
      val minToRaise = parsed.hcursor.downField("minToRaise").as[Long].getOrElse(throw new Throwable("minToRaise not Found"))
      val erg = box.hcursor.downField("value").as[Long].getOrElse(throw new Throwable("BoxId Not Found"))
      val charityAddr = parsed.hcursor.downField("charityAddr").as[String].getOrElse(throw new Throwable("minToRaise not Found"))
      val raffles = s"""{
           | "code" : 200,
           | "id" : "$id",
           | "name" : "$name",
           | "description" : "$description",
           | "deadline" : $deadline,
           | "erg" : $erg,
           | "charity_addr" : "$charityAddr",
           | "winner_percent" : $winnerPercent,
           | "charity_percent" : $charityPercent,
           | "min" : $minToRaise
            }""".stripMargin
      Ok(raffles).as("application/json")
    } catch {
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
  def addRaffle:Action[Json] = Action(circe.json) { implicit request =>
    try {
      val winnerPercent: Long = request.body.hcursor.downField("winnerPercent").as[Long].getOrElse(throw new Throwable("winnerPercent field must exist"))
      val deadlineHeight: Long = request.body.hcursor.downField("deadlineHeight").as[Long].getOrElse(throw new Throwable("deadlineHeight field must exist"))
      val minToRaise: Long = request.body.hcursor.downField("minToRaise").as[Long].getOrElse(throw new Throwable("minToRaise field must exist"))
      val name: String = request.body.hcursor.downField("name").as[String].getOrElse(throw new Throwable("name field must exist"))
      val description: String = request.body.hcursor.downField("description").as[String].getOrElse(throw new Throwable("description field must exist"))
      val charityAddr: String = request.body.hcursor.downField("charityAddr").as[String].getOrElse(throw new Throwable("charityAddr field must exist"))
      val txId = adaptor.addRaffle(winnerPercent, name, description, deadlineHeight, charityAddr, minToRaise)



      Ok(
        s"""{
           |  "txId" : $txId
           |}""".stripMargin
      ).as("application/json")
    } catch {
      case e: Throwable => exception(e)
    }
  }

  def createService() = Action {implicit request: Request[AnyContent] =>
    try {
      val boxId = adaptor.createService()
      Ok(
        s"""{
           |  txId : $boxId
           |}""".stripMargin
      ).as("application/json")
    } catch {
      case e: Throwable => exception(e)
    }
  }

  def donateToId:Action[Json] = Action(circe.json) { implicit request =>
    val raffleId: String = request.body.hcursor.downField("id").as[String].getOrElse(throw new Throwable("id field must exist"))
    val walletAddr: String = request.body.hcursor.downField("walletAddr").as[String].getOrElse(throw new Throwable("walletAddr field must exist"))
    val erg: Long = request.body.hcursor.downField("erg").as[Long].getOrElse(throw new Throwable("erg field must exist"))
    val proxyAddress = adaptor.findProxyAddress(walletAddr, raffleId)
    adaptor.makeDonateTransDelay(walletAddr, proxyAddress, raffleId)
    Ok(
      s"""{
         | "code": 200,
         | "deadline": 900,
         | "address": "$proxyAddress",
         | "erg": $erg
         |}""".stripMargin
    ).as("application/json")
  }

  def servicePercent() = Action {
    val p = Configs.servicePercent
    Ok(
      s"""{
        | "code" : 200,
        | "z" : $p
        |}""".stripMargin).as("application/json")
  }
}
