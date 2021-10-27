package controllers

import dao.{CreateReqDAO, DonateReqDAO, RaffleCacheDAO, TxCacheDAO}
import helpers.{Configs, Utils, internalException}
import io.circe.Json
import io.circe.generic.codec.DerivedAsObjectCodec.deriveCodec
import io.circe.parser
import network.{Client, Explorer}
import raffle.{Addresses, CreateReqUtils, DonateReqUtils, RaffleUtils, raffleStatus}
import raffle.raffleStatus._
import play.api.Logger
import play.api.libs.circe.Circe
import play.api.mvc._

import javax.inject._
import models.{CreateReq, DonateReq, Raffle, TxCache}

import scala.collection.mutable.Seq


@Singleton
class HomeController @Inject()(assets: Assets, addresses: Addresses, explorer: Explorer, donateReqUtils: DonateReqUtils,
                               client: Client, createReqUtils: CreateReqUtils, raffleUtils: RaffleUtils, utils: Utils,
                               createReqDAO: CreateReqDAO, donateReqDAO: DonateReqDAO, raffleCacheDAO: RaffleCacheDAO,
                               txCacheDAO: TxCacheDAO, val controllerComponents: ControllerComponents) extends BaseController
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


  def getRaffles(sorting: String, status: String, offset: Int, limit: Int) = Action { implicit request: Request[AnyContent] =>
    try {
      val result = {
        val raffles = raffleCacheDAO.selectRaffles(status, sorting, offset, Math.min(offset, 100))
        val serializedRaffles = raffles._1.map(raffle => {
          Json.fromFields(List(
            ("id", Json.fromString(raffle.tokenId)),
            ("name", Json.fromString(raffle.name)),
            ("description", Json.fromString(raffle.description)),
            ("deadline", Json.fromLong(raffle.deadlineHeight)),
            ("picture", parser.parse(raffle.picLinks).getOrElse(Json.fromValues(List[Json]()))),
            ("erg", Json.fromLong(raffle.raised)),
            ("goal", Json.fromLong(raffle.goal)),
            ("status", Json.fromString(raffleStatus.apply(raffle.state).toString)),
            ("donatedPeople", Json.fromLong(raffle.participants))
          ))
        })
        Json.fromFields(List(
          ("items", Json.fromValues(serializedRaffles.toList)),
          ("total", Json.fromInt(raffles._2)),
        ))
      }
      Ok(result.toString()).as("application/json")
    } catch {
      case e: Throwable =>
        logger.error(utils.getStackTraceStr(e))
        throw new internalException
    }
  }


  def getRafflesByTokenId(tokenId: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      val savedRaffle = raffleCacheDAO.byTokenId(tokenId)
      if(savedRaffle.state == active.id) {
        val result = try {
          val raffle = Raffle(raffleUtils.getRaffleBoxByTokenId(tokenId))
          val savedRaffle = raffleCacheDAO.byTokenId(tokenId)
          raffleCacheDAO.updateRaised(savedRaffle.id, raffle.raised, raffle.tickets)

          Json.fromFields(List(
            ("id", Json.fromString(raffle.tokenId)),
            ("name", Json.fromString(raffle.name)),
            ("description", Json.fromString(raffle.description)),
            ("deadline", Json.fromLong(raffle.deadlineHeight)),
            ("goal", Json.fromLong(raffle.goal)),
            ("picture", parser.parse(raffle.picLinks).getOrElse(Json.fromValues(List[Json]()))),
            ("charity", Json.fromString(raffle.charityAddr)),
            ("percent", Json.fromFields(List(
              ("charity", Json.fromLong(raffle.charityPercent)),
              ("winner", Json.fromLong(raffle.winnerPercent)),
              ("service", Json.fromLong(raffle.serviceFee))
            ))),
            ("ticket", Json.fromFields(List(
              ("price", Json.fromLong(raffle.ticketPrice)),
              ("sold", Json.fromLong(raffle.tickets)),
              ("erg", Json.fromLong(raffle.raised))
            ))),
            ("donatedPeople", Json.fromLong(savedRaffle.participants)),
            ("status", Json.fromString(raffleStatus.apply(savedRaffle.state).toString)),
            ("txFee", Json.fromLong(Configs.fee))
          ))
        } catch {
          case e: internalException => throw e
          case e: Throwable =>
            logger.error(utils.getStackTraceStr(e))
            throw e
        }
        Ok(result.toString()).as("application/json")
      } else {
        val result = Json.fromFields(List(
          ("id", Json.fromString(savedRaffle.tokenId)),
          ("name", Json.fromString(savedRaffle.name)),
          ("description", Json.fromString(savedRaffle.description)),
          ("deadline", Json.fromLong(savedRaffle.deadlineHeight)),
          ("goal", Json.fromLong(savedRaffle.goal)),
          ("picture", parser.parse(savedRaffle.picLinks).getOrElse(Json.fromValues(List[Json]()))),
          ("charity", Json.fromString(savedRaffle.charityAddr)),
          ("percent", Json.fromFields(List(
            ("charity", Json.fromLong(savedRaffle.charityPercent)),
            ("winner", Json.fromLong(100 - savedRaffle.charityPercent - savedRaffle.serviceFee)),
            ("service", Json.fromLong(savedRaffle.serviceFee))
          ))),
          ("ticket", Json.fromFields(List(
            ("price", Json.fromLong(savedRaffle.ticketPrice)),
            ("sold", Json.fromLong(savedRaffle.tickets)),
            ("erg", Json.fromLong(savedRaffle.raised))
          ))),
          ("donatedPeople", Json.fromLong(savedRaffle.participants)),
          ("status", Json.fromString(raffleStatus.apply(savedRaffle.state).toString)),
          ("txFee", Json.fromLong(Configs.fee))
        ))
        Ok(result.toString()).as("application/json")
      }
    }
    catch {
      case e: Throwable => exception(e)
    }
  }

  def getTickets(tokenId: String, walletAddr: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try{
      val result = raffleUtils.userTickets(tokenId, walletAddr)
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
      val goal: Long = request.body.hcursor.downField("goal").as[Long].getOrElse(throw new Throwable("goal field must exist"))
      val deadlineHeight: Long = request.body.hcursor.downField("deadline").as[Long].getOrElse(throw new Throwable("deadlineHeight field must exist"))
      val charityPercent: Int = request.body.hcursor.downField("charityPercent").as[Int].getOrElse(throw new Throwable("charityPercent field must exist"))
      val charityAddr: String = request.body.hcursor.downField("charity").as[String].getOrElse(throw new Throwable("charity field must exist"))
      val walletAddr: String = request.body.hcursor.downField("wallet").as[String].getOrElse(throw new Throwable("wallet field must exist"))
      val ticketPrice: Long = request.body.hcursor.downField("ticketPrice").as[Long].getOrElse(throw new Throwable("ticketPrice field must exist"))
      val captcha: String = request.body.hcursor.downField("recaptcha").as[String].getOrElse("")
      if(Configs.recaptchaKey != "not-set") utils.verifyRecaptcha(captcha)
      // TODO: Add pictures

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

      val createResult = createReqUtils.CreateRaffleProxyAddress(walletAddr, charityPercent, name, description, deadlineHeight + client.getHeight, charityAddr, goal, ticketPrice)
      val paymentAddress = createResult._1
      val requestId = createResult._2
      val amount = Configs.fee * 4
      val delay = Configs.creationDelay

      val result = Json.fromFields(List(
        ("deadline", Json.fromLong(delay)),
        ("address", Json.fromString(paymentAddress)),
        ("erg", Json.fromLong(amount)),
        ("requestId", Json.fromLong(requestId))
      ))
      Ok(result.toString()).as("application/json")
    } catch {
      case e: Throwable => exception(e)
    }
  }

  def createReqStatus(id: Long): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      var req: CreateReq = null
      try {
        req = createReqDAO.byId(id)
      } catch{
        case _:Throwable => throw new Throwable("No request found with this id")
      }
      val state: String = {
        if(req.state == 0 && !req.deleted) "waiting"
        else if(req.state == 1 && !req.deleted) "createdWaiting"
        else if(req.state != 2 && req.deleted) "expired"
        else "done"
      }
      val result = Json.fromFields(List(("status", Json.fromString(state))))
      Ok(result.toString()).as("application/json")
    } catch{
      case e:Throwable => exception(e)
    }
  }

  def donateReqStatus(tokenId: String, id: Long): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      var req: DonateReq = null
      try {
        req = donateReqDAO.byId(id)
      } catch{
        case _:Throwable => throw new Throwable("No request found with this id")
      }
      val state: String = {
        if(req.state == 0 && !req.deleted) "waiting"
        else if(req.state == 1 && !req.deleted) "createdWaiting"
        else if(req.state != 2 && req.deleted) "expired"
        else "done"
      }
      val result = Json.fromFields(List(("status", Json.fromString(state))))
      Ok(result.toString()).as("application/json")
    } catch{
      case e:Throwable => exception(e)
    }
  }


  def donateToId(tokenId: String): Action[Json] = Action(circe.json) { implicit request =>
    try {
      val walletAddr: String = request.body.hcursor.downField("wallet").as[String].getOrElse(throw new Throwable("walletAddr field must exist"))
      val ticketCounts: Long = request.body.hcursor.downField("ticketCounts").as[Long].getOrElse(throw new Throwable("erg field must exist"))
      val captcha: String = request.body.hcursor.downField("recaptcha").as[String].getOrElse("")
      if(Configs.recaptchaKey != "not-set") utils.verifyRecaptcha(captcha)

      utils.validateAddress(walletAddr, "wallet")
      utils.validateTicketCounts(ticketCounts)

      val response = donateReqUtils.findProxyAddress(walletAddr, tokenId, ticketCounts)
      val paymentAddress = response._1
      val fee = response._2
      val requestId = response._3
      val deadline = Configs.creationDelay

      val result = Json.fromFields(List(
        ("deadline", Json.fromLong(deadline)),
        ("address", Json.fromString(paymentAddress)),
        ("erg", Json.fromLong(fee)),
        ("requestId", Json.fromLong(requestId))
      ))
      Ok(result.toString()).as("application/json")
    } catch {
      case e: Throwable => exception(e)
    }
  }

  def raffleTransactions(tokenId: String, offset: Int, limit: Int): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      val result = {
        val txs = txCacheDAO.byTokenId(tokenId, offset, limit)
        var tmpTxs: scala.Seq[TxCache] = Seq.empty
        // TODO: condition (Add charityTx) is a fake tx should be remove in production
        if (txs._3 != 0 && txs._1.nonEmpty) {
          val charityTx = txs._2.head.copy(txType = "Charity")
          tmpTxs = txs._1 :+ charityTx
        }
        tmpTxs ++= txs._2
        val transactions = tmpTxs.take(limit).map(tx => {
          Json.fromFields(List(
            ("id", Json.fromString(tx.txId)),
            ("address", Json.fromString(tx.wallerAdd)),
            ("type", Json.fromString(tx.txType)),
            ("tickets", Json.fromLong(tx.tokenCount)),
            ("link", Json.fromString(utils.getTransactionFrontLink(tx.txId)))
          ))
        })
        Json.fromFields(List(
          ("items", Json.fromValues(transactions.toList)),
          ("total", Json.fromInt(txs._4))
        ))
      }
      Ok(result.toString()).as("application/json")
    } catch{
      case e: Throwable =>
        logger.error(utils.getStackTraceStr(e))
        exception(e)
    }
  }

  def walletTickets(walletAdd: String, offset: Int, limit: Int): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      val result = {
        val queryResult = txCacheDAO.getDonationsByWalletAddr(walletAdd, offset, Math.min(limit,100))
        val totalRecords: Long = queryResult._2
        val donations = queryResult._1.map(ticket => {
          val raffle = raffleCacheDAO.byTokenId(ticket._1)
          Json.fromFields(List(
            ("id", Json.fromString(raffle.tokenId)),
            ("name", Json.fromString(raffle.name)),
            ("description", Json.fromString(raffle.description)),
            ("deadline", Json.fromLong(raffle.deadlineHeight)),
            ("picture", parser.parse(raffle.picLinks).getOrElse(Json.fromValues(List[Json]()))),
            ("erg", Json.fromLong(raffle.raised)),
            ("goal", Json.fromLong(raffle.goal)),
            ("status", Json.fromString(raffleStatus.apply(raffle.state).toString)),
            ("tickets", Json.fromLong(ticket._2.getOrElse(0))),
          ))
        })
        Json.fromFields(List(
          ("items", Json.fromValues(donations.toList)),
          ("total", Json.fromLong(totalRecords))
        ))
      }
      Ok(result.toString()).as("application/json")
    } catch{
      case e: Throwable =>
        // TODO
        exception(e)
    }
  }

  def walletWins(walletAdd: String, offset: Int, limit: Int): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      val result = {
        val queryResult = txCacheDAO.winnerByWalletAddr(walletAdd, offset, Math.min(limit,100))
        val tickets = queryResult._1
        val totalRecords: Long = queryResult._2

        val wins = tickets.map(ticket => {
          val raffle = raffleCacheDAO.byTokenId(ticket.tokenId)
          Json.fromFields(List(
            ("id", Json.fromString(raffle.tokenId)),
            ("name", Json.fromString(raffle.name)),
            ("description", Json.fromString(raffle.description)),
            ("deadline", Json.fromLong(raffle.deadlineHeight)),
            ("picture", parser.parse(raffle.picLinks).getOrElse(Json.fromValues(List[Json]()))),
            ("erg", Json.fromLong(raffle.raised)),
            ("goal", Json.fromLong(raffle.goal)),
            ("tickets", Json.fromLong(ticket.tokenCount)),
            ("link", Json.fromString(utils.getTransactionFrontLink(ticket.txId)))
          ))
        })
        Json.fromFields(List(
          ("items", Json.fromValues(wins.toList)),
          ("total", Json.fromLong(totalRecords))
        ))
      }
      Ok(result.toString()).as("application/json")
    } catch{
      case e: Throwable => logger.error(utils.getStackTraceStr(e))
        // TODO
        exception(e)
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

  def info(): Action[AnyContent] = Action {
    val key = Configs.recaptchaPubKey
    var required = true
    if(key == "not-set") required = false
    val currentHeight = client.getHeight

    val result = Json.fromFields(List(
      ("pubKey", Json.fromString(key)),
      ("required", Json.fromBoolean(required)),
      ("height", Json.fromLong(currentHeight))
    ))
    Ok(result.toString()).as("application/json")
  }

  def support(): Action[AnyContent] = Action {
    val result = Json.fromFields(List(
      ("state", Json.fromString("success"))
    ))
    Ok(result.toString()).as("application/json")
  }
}
