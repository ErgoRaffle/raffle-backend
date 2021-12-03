package raffle

import dao.RaffleCacheDAO
import helpers.{Configs, Utils, connectionException, failedTxException, internalException, noRaffleException, parseException}
import io.circe.Json

import javax.inject.Inject
import models.{Raffle, Ticket}
import network.{Client, Explorer}
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.appkit.{Address, ErgoClientException, ErgoToken, InputBox}
import play.api.Logger
import raffle.raffleStatus.active

import scala.collection.JavaConverters._
import scala.collection.mutable.Seq

object raffleStatus extends Enumeration{
  type raffleStatus = Value
  val unknown: Value = Value(0, "unknown")
  val inactive: Value = Value(1, "inactive")
  val active: Value = Value(2, "active")
  val succeed: Value = Value(3, "succeed")
  val failed: Value = Value(4, "failed")
}

object txType extends Enumeration{
  type raffleStatus = Value
  val winner: Value = Value(0, "winner")
  val charity: Value = Value(1, "charity")
  val ticket: Value = Value(2, "ticket")
  val refund: Value = Value(3, "refund")
  val unknownType: Value = Value(4, "unknown")
}

class RaffleUtils @Inject()(client: Client, explorer: Explorer, addresses: Addresses, utils: Utils, raffleCacheDAO: RaffleCacheDAO) {

  private val logger: Logger = Logger(this.getClass)

  def userTickets(raffleId: String, wallerAdd: String): (List[Ticket], Long, Long) ={
    try {
      var offset = 0
      var tickets = getTicketBoxes(raffleId, offset)
      var selectedTickets: List[Ticket] = List[Ticket]()
      var totalTickets: Long = 0
      var totalRecords: Long = 0
      while (tickets != null && tickets.nonEmpty) {
        for (ticketBox <- tickets) {
          val ticket = Ticket(ticketBox)
          if (ticket.walletAddress == wallerAdd) {
            selectedTickets = selectedTickets :+ ticket
            totalTickets += ticket.tokenCount
            totalRecords += 1
          }
        }
        offset += 100
        tickets = getTicketBoxes(raffleId, offset)
      }
      (selectedTickets, totalTickets, totalRecords)
    } catch {
      case e: connectionException => throw e
      case _: internalException => throw internalException()
      case e: Throwable =>
        logger.error(utils.getStackTraceStr(e))
        throw new internalException
    }
  }

  def raffleByTokenId(tokenId: String): (Raffle, Long, String) = try{
    val savedRaffle = raffleCacheDAO.byTokenId(tokenId)
    var raffle: Raffle = null
    if(savedRaffle.state == active.id) raffle = Raffle(getRaffleBoxByTokenId(tokenId))
    else raffle = Raffle(savedRaffle)
    (raffle, savedRaffle.participants, raffleStatus.apply(savedRaffle.state).toString)
  } catch{
    case e: connectionException => throw e
    case _: java.util.NoSuchElementException => throw noRaffleException()
    case e: Throwable =>
      logger.error(utils.getStackTraceStr(e))
      throw internalException()
  }

  def getRaffleBoxByTokenId(tokenId: String): Json = {
    var boxes: Json = null
    var raffleBox: Json = null
    var offset = 0
    var total: Int = 1
    while (offset < total && raffleBox == null) {
      try {
        boxes = explorer.getUnspentTokenBoxes(Configs.token.service, offset, 100)
        total = boxes.hcursor.downField("total").as[Int].getOrElse(0)
        raffleBox = boxes.hcursor.downField("items").as[Seq[Json]].getOrElse(throw parseException())
          .filter(raffle => {
            val assets = raffle.hcursor.downField("assets").as[Seq[Json]].getOrElse(throw parseException())
            assets.size > 1 && assets(1).hcursor.downField("tokenId").as[String].getOrElse("") == tokenId
          }).head
      } catch{
        case e: connectionException => throw e
        case _: java.util.NoSuchElementException => offset += 100
        case e: parseException =>
          logger.error(e.getMessage)
          throw internalException()
        case e: Throwable =>
          logger.error(utils.getStackTraceStr(e))
          throw internalException()
      }
    }
    raffleBox
  }

  def raffleParticipants(tokenId: String): Long={
    try {
      var result = 0
      var offset = 0
      var tickets = getTicketBoxes(tokenId, offset)
      while (tickets != null && tickets.nonEmpty) {
        result += tickets.size
        offset += 100
        tickets = getTicketBoxes(tokenId, offset)
      }
      result
    } catch{
      case e: connectionException => throw e
      case _: java.lang.NullPointerException => 0
      case _: parseException => throw internalException()
      case e: Throwable =>
        logger.error(utils.getStackTraceStr(e))
        throw internalException()
    }
  }

  def getTicketBoxes(tokenId: String, offset: Int): Seq[Json] ={
    try {
      explorer.getAllTokenBoxes(tokenId, offset, 100)
        .hcursor.downField("items").as[Seq[Json]].getOrElse(throw new parseException)
        .filter(_.hcursor.downField("address").as[String].getOrElse("") == addresses.ticketAddress.toString)
    } catch{
      case e: connectionException => throw e
      case e: parseException =>
        logger.error(e.getMessage)
        throw internalException()
      case _: java.util.NoSuchElementException => throw internalException()
      case e: Throwable =>
        logger.error(utils.getStackTraceStr(e))
        throw internalException()
    }
  }

  def getAllRaffleBoxes(offset: Int): List[Json] = try {
    explorer.getUnspentTokenBoxes(Configs.token.service, offset, 100)
      .hcursor.downField("items").as[List[Json]].getOrElse(throw parseException())
      .filter(raffle => {
        val assets = raffle.hcursor.downField("assets").as[Seq[Json]].getOrElse(throw parseException())
        assets.size > 1 && assets.head.hcursor.downField("tokenId").as[String].getOrElse("") == Configs.token.service
      })
  } catch {
    case e: connectionException => throw e
    case e: parseException =>
      logger.error(e.getMessage)
      throw internalException()
    case _: java.util.NoSuchElementException => throw noRaffleException()
    case e: Throwable =>
      logger.error(utils.getStackTraceStr(e))
      throw internalException()
  }

  def getWinnerBox(tokenId: String): Json = {
    var boxes: Json = null
    var winnerBox: Json = null
    var offset = 0
    var total: Int = 1
    while (offset < total && winnerBox == null) {
      try {
        boxes = explorer.getAllTokenBoxes(tokenId, offset, 100)
        total = boxes.hcursor.downField("total").as[Int].getOrElse(0)
        winnerBox = boxes.hcursor.downField("items").as[Seq[Json]].getOrElse(throw parseException())
          .filter(_.hcursor.downField("address").as[String].getOrElse("") == addresses.raffleWinnerAddress.toString).head
      } catch{
        case e: connectionException => throw e
        case _: java.util.NoSuchElementException => offset += 100
        case _: parseException => throw new internalException
        case e: Throwable =>
          logger.error(utils.getStackTraceStr(e))
          throw new internalException
      }
    }
    winnerBox
  }

  def refundBoxes(boxes: List[InputBox], address: Address): String = {
    try {
      client.getClient.execute(ctx => {
        val prover = ctx.newProverBuilder()
        .build()
        var value: Long = 0L
        var tokens: Seq[ErgoToken] = Seq()
        boxes.foreach(box => {
          value += box.getValue
          if (box.getTokens.size() > 0) tokens = tokens ++ box.getTokens.asScala
        })
        val txB = ctx.newTxBuilder()
        var outB = txB.outBoxBuilder()
        outB = outB.value(value - Configs.fee)
        outB = outB.contract(new ErgoTreeContract(address.getErgoAddress.script))

        if (tokens.nonEmpty) outB = outB.tokens(tokens: _*)
        val tx = txB.boxesToSpend(boxes.asJava)
            .fee(Configs.fee)
            .outputs(outB.build())
            .sendChangeTo(address.getErgoAddress)
            .build()
        val signed = prover.sign(tx)
        val txId = ctx.sendTransaction(signed)
        if (txId == null) throw failedTxException(s"refund failed for address ${address.toString}")
        else txId.replaceAll("\"", "")
      })
    } catch {
      case e: failedTxException =>
        logger.warn(e.getMessage)
        throw failedTxException()
      case e: ErgoClientException =>
        logger.warn(e.getMessage)
        throw connectionException()
      case e:Throwable =>
        logger.error(utils.getStackTraceStr(e))
        throw new Throwable("refunding failed")
    }
  }

}
