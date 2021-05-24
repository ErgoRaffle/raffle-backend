package controllers

import javax.inject._
import play.api.mvc._
import io.circe.Json
import play.api.libs.circe.Circe
import scalaj.http._
import io.circe.jawn
import helpers.Utils
import scorex.crypto.hash._

@Singleton
class HomeController @Inject()(val controllerComponents: ControllerComponents, utils: Utils) extends BaseController with Circe {

  def index() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index())
  }

  def exception(e: Throwable): Result = {
    BadRequest(s"""{"success": false, "message": "${e.getMessage}"}""").as("application/json")
  }
  /*
  Input format should be like:
    {\"data1\": \"hello\", \"data2\": \"to\", \"data3\": \"the\", \"data4\": \"world\"}"
  */
  def getHash(): Action[Json] = Action(circe.json) { implicit request =>
    try {
      val str1 = request.body.hcursor.downField("data1").as[String].getOrElse(throw new Throwable("data1 field must exist"))
      val str2 = request.body.hcursor.downField("data2").as[String].getOrElse(throw new Throwable("data2 field must exist"))
      val str3 = request.body.hcursor.downField("data3").as[String].getOrElse(throw new Throwable("data3 field must exist"))
      val str4 = request.body.hcursor.downField("data4").as[String].getOrElse(throw new Throwable("data4 field must exist"))
      val str = str1 + str2 + str3 + str4

      val SHA1 = utils.SHA1(str)
      val Blake256 = Blake2b256(str)
      Ok(s"""{"SHA1": "${SHA1}", "Blake2b256": "${Blake256}"}""").as("application/json")

    } catch {
      case e: Throwable => exception(e)
    }
  }
  def getById(tokenId: String) = Action {
    try {
      val response: HttpResponse[String] = Http(utils.getAPILink() + tokenId).header("Accept", "application/json").asString
      val res1 = jawn.parse(response.body).getOrElse(throw new Throwable("parse error"))
      val res2 = res1.hcursor.downField("items").as[List[Json]].getOrElse(throw new Throwable("parse error"))
      val boxes = res2.map("\"" + _.hcursor.downField("boxId").as[String].getOrElse(throw new Throwable("parse error")) + "\"").mkString(", ")

      Ok(
        s"""{
           |  "tokenId": "${tokenId}",
           |  "boxIds" : [${boxes}]
           |}""".stripMargin
      ).as("application/json")
    } catch {
      case e: Throwable => exception(e)
    }
  }
}
