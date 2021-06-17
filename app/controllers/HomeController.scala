package controllers

import javax.inject._
import play.api.mvc._
import io.circe.Json
import play.api.libs.circe.Circe
import scalaj.http._
import io.circe.jawn
import helpers.Utils
import network.{Client, Explorer}
import scorex.crypto.hash._
import org.ergoplatform.appkit._
import org.ergoplatform.appkit.Address
import org.ergoplatform.ErgoAddress
import org.ergoplatform.appkit.impl.ErgoTreeContract
import raffle.Adaptor


@Singleton
class HomeController @Inject()(adaptor: Adaptor, explorer: Explorer, client: Client, val controllerComponents: ControllerComponents, utils: Utils) extends BaseController
  with Circe {

  def index() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index())
  }

  def exception(e: Throwable): Result = {
    BadRequest(s"""{"success": false, "message": "${e.getMessage}"}""").as("application/json")
  }



  def boxToRaffle(boxId: String): String = {
    try {
      val response: HttpResponse[String] =
        Http(utils.getAPILink("getApiV1BoxesP1") + boxId)
          .header("Accept", "application/json").asString
      val box = jawn.parse(response.body).getOrElse(throw new Throwable("Body Not Found"))
      val register = box.hcursor.downField("additionalRegisters")
        .downField("R9").as[Json].getOrElse(throw new Throwable("parse error"))
      val id = box.hcursor.downField("boxId").as[String].getOrElse(throw new Throwable("parse error"))
      val name = register.hcursor.downField("name").as[String].getOrElse(throw new Throwable("parse error"))
      val description = register.hcursor.downField("description").as[String].getOrElse(throw new Throwable("parse error"))
      val deadline = register.hcursor.downField("deadline").as[String].getOrElse(throw new Throwable("parse error"))
      val erg = box.hcursor.downField("value").as[Long].getOrElse(throw new Throwable("parse error"))
      val organizer_addr = register.hcursor.downField("organizer_addr").as[String].getOrElse(throw new Throwable("parse error"))
      val min = register.hcursor.downField("min").as[Long].getOrElse(throw new Throwable("parse error"))
      val charity_addr =register.hcursor.downField("charity_addr").as[String].getOrElse(throw new Throwable("parse error"))
      val result = s"""{
                      |  "id" : "${id}"
                      |  "name" : "${name}"
                      |  "description" : "${description}"
                      |  "deadline" : "${deadline}"
                      |  "erg" : ${erg}
                      |  "organizer_addr" : ${organizer_addr}
                      |  "charity_addr" : ${charity_addr}
                      |  "min" : ${min}
                      |}""".stripMargin
      return result
    } catch {
      case e: Throwable => exception(e)
      return ""
    }
  }

  def getRaffles(offset: Int, limit: Int) = Action {
    try {
      val boxes = explorer.getUnspentTokenBoxes(utils.getTokenId(), limit, offset)
      val items = boxes.hcursor.downField("items").as[List[Json]].getOrElse(throw new Throwable("parse error"))
      val result = items.map(m => boxToRaffle(m.hcursor.downField("boxId").as[String].getOrElse(throw new Throwable("BoxId Not Found"))))
        .mkString(", ")
      Ok(
        s"""{
           | items : [${result}]
           | total : ${result.size}
           |}""".stripMargin
      ).as("application/json")
    } catch {
      case e: Throwable => exception(e)
    }
  }

  def getRafflesById(raffleId: String) = Action {
    try {
      Ok(
        boxToRaffle(raffleId)
      ).as("application/json")
    } catch {
      case e: Throwable => exception(e)
    }
  }

  def addRaffle(winnerPercent: Long, name: String, description: String, deadlineHeight: Long, charityAddr: String, minToRaise: Long) = Action {
//    try {
      val txId = adaptor.addRaffle(winnerPercent, name, description, deadlineHeight, charityAddr, minToRaise)
      Ok(
        s""""{
           |  txId : "$txId"
           |}""".stripMargin
      ).as("application/json")
//    } catch {
//      case e: Throwable => exception(e)
//    }
  }

  def createService() = Action {
    try {
      val txId = adaptor.createService()
      Ok(
        s""""{
           |  txId : "$txId"
           |}""".stripMargin
      ).as("application/json")
    } catch {
      case e: Throwable => exception(e)
    }
  }
//  def donateToId(raffleId: String, value: Int) = Action {
//
//  }

}
