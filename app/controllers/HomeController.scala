package controllers

import dao.{CreateReqDAO, DonateReqDAO, RaffleCacheDAO, TxCacheDAO}
import helpers.{Configs, Utils, connectionException, internalException, noRaffleException}
import io.circe.{Json, parser}
import io.circe.generic.codec.DerivedAsObjectCodec.deriveCodec
import network.Client
import raffle.{CreateReqUtils, DonateReqUtils, RaffleUtils, raffleStatus, txType}
import raffle.raffleStatus._
import play.api.Logger
import play.api.libs.circe.Circe
import play.api.mvc._
import javax.inject._
import models.{CreateReq, DonateReq, TxCache}
import org.ergoplatform.appkit.Address

import scala.collection.mutable.{ListBuffer, Seq}
import io.kinoplan.emailaddress._


@Singleton
class HomeController @Inject()(assets: Assets, donateReqUtils: DonateReqUtils, client: Client, createReqUtils: CreateReqUtils,
                               raffleUtils: RaffleUtils, utils: Utils, createReqDAO: CreateReqDAO, donateReqDAO: DonateReqDAO,
                               raffleCacheDAO: RaffleCacheDAO, txCacheDAO: TxCacheDAO,
                               val controllerComponents: ControllerComponents) extends BaseController
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

  /**
   * @param sorting defining the sorting key, raffle sorting can be based on "createtime", "deadline" or "activity"
   * @param status filtering raffles with specified status, it can be "all", "active", "succeed" or "failed"
   * @return list of raffle information based on the requirements and query limit offset
   */
  def getRaffles(sorting: String, status: String, offset: Int, limit: Int) = Action { implicit request: Request[AnyContent] =>
    try {
      val result = {
        val raffles = raffleCacheDAO.selectRaffles(status, sorting, offset, Math.min(limit, 100))
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
        exception(e)
    }
  }

  /**
   * @param tokenId raffle token id
   * @return raffle information with the specified token id
   */
  def getRafflesByTokenId(tokenId: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      val query = raffleUtils.raffleByTokenId(tokenId)
      val raffle = query._1
      val participants = query._2
      val status = query._3
      val result = Json.fromFields(List(
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
            ("donatedPeople", Json.fromLong(participants)),
            ("status", Json.fromString(status)),
            ("txFee", Json.fromLong(Configs.fee))
          ))
      Ok(result.toString()).as("application/json")
    }
    catch {
      case e: connectionException => exception(e)
      case e: noRaffleException => exception(e)
      case e: internalException => exception(e)
      case e: Throwable =>
        logger.error(utils.getStackTraceStr(e))
        exception(e)
    }
  }

  /**
   * @param tokenId raffle token id
   * @param walletAddr user wallet address
   * @return user tickets on the specified raffle
   * *this api should be used for active raffles only
   */
  def getTickets(tokenId: String, walletAddr: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try{
      val query = raffleUtils.userTickets(tokenId, walletAddr)
      val ticketList: ListBuffer[Json] = ListBuffer()
      query._1.foreach(ticket =>{
        ticketList += Json.fromFields(List(
          ("id", Json.fromString(ticket.txId)),
          ("tickets", Json.fromLong(ticket.tokenCount)),
          ("link", Json.fromString(utils.getTransactionFrontLink(ticket.txId)))
        ))
      })
      val result = Json.fromFields(List(
        ("items", Json.fromValues(ticketList.toList)),
        ("totalTickets", Json.fromLong(query._2)),
        ("total", Json.fromLong(query._3))
      ))
      Ok(result.toString()).as("application/json")
    }
    catch {
      case e: connectionException => exception(e)
      case e: internalException => exception(e)
      case e: Throwable =>
        logger.error(utils.getStackTraceStr(e))
        exception(e)
    }
  }

  /**
   * creates a new raffle creation request with limited time
   * @return payment address, deadline, the creation fee, and creation request id
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
      val picLinks: List[String] = request.body.hcursor.downField("picture").as[Seq[String]].getOrElse(throw new Throwable("picture is required")).toList
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

      val createResult = createReqUtils.CreateRaffleProxyAddress(walletAddr, charityPercent, name, description, deadlineHeight + client.getHeight, charityAddr, goal, ticketPrice, picLinks)
      val paymentAddress = createResult._1
      val requestId = createResult._2
      val delay = Configs.creationDelayTime

      val result = Json.fromFields(List(
        ("deadline", Json.fromLong(delay)),
        ("address", Json.fromString(paymentAddress)),
        ("erg", Json.fromLong(Configs.creationFee)),
        ("requestId", Json.fromLong(requestId))
      ))
      Ok(result.toString()).as("application/json")
    } catch {
      case e: Throwable => exception(e)
    }
  }

  /**
   * @param id the request id
   * @return status of the request with specified id
   */
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

  /**
   * @param id the request id
   * @return status of the request with specified id
   */
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

  /**
   * creates a new donation request with limited time
   * @return payment address, deadline, the donation fee, and the donation request id
   */
  def donateToId(tokenId: String): Action[Json] = Action(circe.json) { implicit request =>
    try {
      val walletAddr: String = request.body.hcursor.downField("walletAddr").as[String].getOrElse(throw new Throwable("walletAddr field must exist"))
      val ticketCounts: Long = request.body.hcursor.downField("ticketCounts").as[Long].getOrElse(throw new Throwable("ticketCounts field must exist"))
      val captcha: String = request.body.hcursor.downField("recaptcha").as[String].getOrElse("")
      if(Configs.recaptchaKey != "not-set") utils.verifyRecaptcha(captcha)
      try {
        if (raffleCacheDAO.byTokenId(tokenId).state != active.id) throw new Throwable("This raffle is finished earlier")
      } catch{
        case _: java.util.NoSuchElementException => throw new Throwable("No raffle exist with this id")
      }

      utils.validateAddress(walletAddr, "wallet")
      utils.validateTicketCounts(ticketCounts)

      val response = donateReqUtils.findProxyAddress(walletAddr, tokenId, ticketCounts)
      val paymentAddress = response._1
      val fee = response._2
      val requestId = response._3
      val deadline = Configs.creationDelayTime

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

  /**
   * @param tokenId raffle token id
   * @return winner, charity and user ticket transactions belonging to the specified raffle
   * *this api should be used for finished raffles only
   */
  def raffleTransactions(tokenId: String, offset: Int, limit: Int): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      val result = {
        val raffleState = raffleCacheDAO.byTokenId(tokenId).state
        val txs = txCacheDAO.byTokenId(tokenId, offset, limit, raffleState)
        var tmpTxs: scala.Seq[TxCache] = Seq.empty
        tmpTxs ++= txs._1
        val transactions = tmpTxs.take(limit).map(tx => {
          Json.fromFields(List(
            ("id", Json.fromString(tx.txId)),
            ("address", Json.fromString(tx.wallerAdd)),
            ("type", Json.fromString(txType.apply(tx.txType).toString)),
            ("tickets", Json.fromLong(tx.tokenCount)),
            ("link", Json.fromString(utils.getTransactionFrontLink(tx.txId)))
          ))
        })
        Json.fromFields(List(
          ("items", Json.fromValues(transactions.toList)),
          ("total", Json.fromInt(txs._2))
        ))
      }
      Ok(result.toString()).as("application/json")
    } catch{
      case e: Throwable =>
        logger.error(utils.getStackTraceStr(e))
        exception(e)
    }
  }

  /**
   * @param walletAddr user wallet address
   * @return user donations on all raffles
   */
  def walletTickets(walletAddr: String, offset: Int, limit: Int): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      val result = {
        val queryResult = txCacheDAO.getDonationsByWalletAddr(walletAddr, offset, Math.min(limit,100))
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
      case _: java.util.NoSuchElementException => exception(new Throwable("No tickets found for this address"))
      case e: Throwable =>
        logger.error(utils.getStackTraceStr(e))
        exception(e)
    }
  }

  /**
   * @param walletAddr user wallet address
   * @return user wins on all raffles
   */
  def walletWins(walletAddr: String, offset: Int, limit: Int): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      val result = {
        val queryResult = txCacheDAO.winnerByWalletAddr(walletAddr, offset, Math.min(limit,100))
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
      case _: java.util.NoSuchElementException => exception(new Throwable("No winner tickets found for this address"))
      case e: Throwable =>
        logger.error(utils.getStackTraceStr(e))
        exception(e)
    }
  }

  /**
   * creates a refund transaction with the input data
   * @return refund txId
   */
  def refundPayments(): Action[Json] = Action(circe.json) { implicit request =>
    try {
      val walletAddress: String = request.body.hcursor.downField("wallet").as[String].getOrElse(throw new Throwable("wallet field must exist"))
      val paymentAddress: String = request.body.hcursor.downField("payment").as[String].getOrElse(throw new Throwable("payment field must exist"))
      val captcha: String = request.body.hcursor.downField("recaptcha").as[String].getOrElse("")
      if(Configs.recaptchaKey != "not-set") utils.verifyRecaptcha(captcha)

      val result = {
        val boxes = client.getAllUnspentBox(Address.create(paymentAddress))
        if (boxes.nonEmpty) {
          val txId = raffleUtils.refundBoxes(boxes, Address.create(walletAddress))
          Json.fromFields(List(
            ("success", Json.fromBoolean(true)),
            ("txId", Json.fromString(txId))
          ))
        }
        else throw new Throwable("There is no box available for this payment address")
      }
      Ok(result.toString()).as("application/json")
    } catch{
      case e: Throwable => exception(e)
    }
  }

  /**
   * @return service information
   */
  def info(): Action[AnyContent] = Action {
    val key = Configs.recaptchaPubKey
    var required = true
    if(key == "not-set") required = false
    val currentHeight = client.getHeight
    val serviceBox = utils.getServiceBox()
    val serviceFee = serviceBox.getRegisters.get(0).getValue.asInstanceOf[Long]
    val supportUrl = Configs.supportUrl

    val result = Json.fromFields(List(
      ("pubKey", Json.fromString(key)),
      ("supportUrl", Json.fromString(supportUrl)),
      ("required", Json.fromBoolean(required)),
      ("height", Json.fromLong(currentHeight)),
      ("serviceFee", Json.fromLong(serviceFee))
    ))
    Ok(result.toString()).as("application/json")
  }

  def support(): Action[AnyContent] = Action {
    val result = Json.fromFields(List(
      ("state", Json.fromString("success"))
    ))
    Ok(result.toString()).as("application/json")
  }

  /**
  * send detail of form contact us trough a webhook
   * @return status of cal webhook
   */
  def contact(): Action[Json] = Action(circe.json) { implicit request =>
    try {
      val email: String = request.body.hcursor.downField("email").as[String].getOrElse(throw new Throwable("email field must exist"))
      if (!EmailAddress.isValid(email)) throw new Throwable("email format invalid")
      val message: String = request.body.hcursor.downField("message").as[String].getOrElse(throw new Throwable("message field must exist"))
      val captcha: String = request.body.hcursor.downField("recaptcha").as[String].getOrElse("")
      if(Configs.recaptchaKey != "not-set") utils.verifyRecaptcha(captcha)

      val result = {
        utils.sendDetailForContactToWebHook(EmailAddress(email), message)
        Json.fromFields(List(
          ("success", Json.fromBoolean(true))
        ))
      }
      Ok(result.toString).as("application/json")
    } catch{
      case e: Throwable => exception(e)
    }
  }
}
