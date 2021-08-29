package controllers

import helpers.{Configs, Utils}
import io.circe.Json
import io.circe.generic.codec.DerivedAsObjectCodec.deriveCodec
import network.{Client, Explorer}
import raffle.{Addresses, CreateReqUtils, DonateReqUtils, RaffleUtils}

import play.api.Logger
import play.api.libs.circe.Circe
import play.api.mvc._

import javax.inject._


@Singleton
class HomeController @Inject()(assets: Assets, addresses: Addresses, explorer: Explorer, donateReqUtils: DonateReqUtils,
                               client: Client, createReqUtils: CreateReqUtils, raffleUtils: RaffleUtils,
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
    logger.warn(e.getMessage)
    BadRequest(s"""{"success": false, "message": "${e.getMessage}"}""").as("application/json")
  }


  def getRaffles(offset: Int, limit: Int) = Action { implicit request: Request[AnyContent] =>
    logger.info("Responding get raffles request")
    try {
      val result = raffleUtils.raffles(offset, limit)
      Ok(result.toString()).as("application/json")
    } catch {
      case e: Throwable => exception(e)
    }
  }


  def getRafflesByTokenId(tokenId: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      logger.info("Responding get raffle request by token id: "+ tokenId)
      val result = raffleUtils.raffleByTokenId(tokenId)
      Ok(result.toString()).as("application/json")
    }
    catch {
      case e: Throwable => exception(e)
    }
  }

  def getTickets(): Action[Json] = Action(circe.json) { implicit request =>
    try{
      val walletAddr: String = request.body.hcursor.downField("walletAddr").as[String].getOrElse(throw new Throwable("wallet address must exist"))
      val raffleId: String = request.body.hcursor.downField("raffleId").as[String].getOrElse(throw new Throwable("raffleId must exist"))
      val result = raffleUtils.userTickets(raffleId, walletAddr)
      Ok(result.toString()).as("application/json")
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
      if(Configs.recaptchaKey != "not-set") utils.verifyRecaptcha(captcha)

      if(name.length > 250) throw new Throwable("Name size limit is 250 characters")
      if(description.length > 1000) throw new Throwable("Description size limit is 250 characters")
      utils.validateErgValue(ticketPrice)
      utils.validateErgValue(goal)
      utils.validateAddress(charityAddr, "charity")
      utils.validateAddress(walletAddr, "wallet")
      utils.validateDeadline(deadlineHeight)

      val serviceBox = utils.getServiceBox()
      val servicePercent = serviceBox.getRegisters.get(0).getValue.asInstanceOf[Long]
      utils.validateCharityPercent(charityPercent, servicePercent)

      val paymentAddress = createReqUtils.CreateRaffleProxyAddress(walletAddr, charityPercent, name, description, deadlineHeight + client.getHeight, charityAddr, goal, ticketPrice)
      val amount = Configs.fee * 4
      val delay = Configs.creationDelay

      val result = Json.fromFields(List(
        ("deadline", Json.fromLong(delay)),
        ("address", Json.fromString(paymentAddress)),
        ("fee", Json.fromLong(amount))
      ))
      Ok(result.toString()).as("application/json")
    } catch {
      case e: Throwable => exception(e)
    }
  }


  def donateToId: Action[Json] = Action(circe.json) { implicit request =>
    try {
      val raffleId: String = request.body.hcursor.downField("id").as[String].getOrElse(throw new Throwable("id field must exist"))
      val walletAddr: String = request.body.hcursor.downField("walletAddr").as[String].getOrElse(throw new Throwable("walletAddr field must exist"))
      val ticketCounts: Long = request.body.hcursor.downField("ticketCounts").as[Long].getOrElse(throw new Throwable("erg field must exist"))
      val captcha: String = request.body.hcursor.downField("captcha").as[String].getOrElse("")
      if(Configs.recaptchaKey != "not-set") utils.verifyRecaptcha(captcha)

      utils.validateAddress(walletAddr, "wallet")
      utils.validateTicketCounts(ticketCounts)

      val response = donateReqUtils.findProxyAddress(walletAddr, raffleId, ticketCounts)
      val paymentAddress = response._1
      val fee = response._2
      val deadline = Configs.creationDelay

      val result = Json.fromFields(List(
        ("deadline", Json.fromLong(deadline)),
        ("address", Json.fromString(paymentAddress)),
        ("fee", Json.fromLong(fee))
      ))
      Ok(result.toString()).as("application/json")
    } catch {
      case e: Throwable => exception(e)
    }
  }

  def servicePercent(): Action[AnyContent] = Action {
    val serviceBox = utils.getServiceBox()
    val p = serviceBox.getRegisters.get(0).getValue.asInstanceOf[Long]

    val result = Json.fromFields(List(
      ("z", Json.fromLong(p))
    ))
    Ok(result.toString()).as("application/json")
  }

  def recaptchaKey(): Action[AnyContent] = Action {
    val key = Configs.recaptchaPubKey
    var required = ""
    if(key == "not-set") required = "false"
    else required = "true"

    val result = Json.fromFields(List(
      ("pubKey", Json.fromString(key)),
      ("required", Json.fromString(required))
    ))
    Ok(result.toString()).as("application/json")
  }

  def getExplorer(): Action[AnyContent] = Action {
    val explorer = Configs.explorerFront

    val result = Json.fromFields(List(
      ("explorerUrl", Json.fromString(explorer))
    ))
    Ok(result.toString()).as("application/json")
  }
}
