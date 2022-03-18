package raffle

import dao.{RaffleCacheDAO, TxCacheDAO}
import helpers.{Configs, Utils, connectionException, internalException, noRaffleException, parseException}

import javax.inject.Inject
import network.{Client, Explorer}
import play.api.Logger
import io.circe.Json
import models.{Raffle, RaffleCache, Ticket}
import org.ergoplatform.appkit.ErgoClientException
import raffle.raffleStatus._

import scala.collection.mutable.Seq


class RaffleCacheUtils @Inject()(client: Client, explorer: Explorer, utils: Utils, addresses: Addresses,
                                 raffleCacheDAO: RaffleCacheDAO, txCacheDAO: TxCacheDAO, raffleUtils: RaffleUtils) {
  private val logger: Logger = Logger(this.getClass)

  def raffleStateByAddress(address: String): Int ={
    if(address == addresses.raffleInactiveAddress.toString) inactive.id
    else if(address == addresses.raffleActiveAddress.toString) active.id
    else if(address == addresses.raffleRedeemAddress.toString) failed.id
    else unknown.id
  }

  def updateRaffle(savedRaffle: RaffleCache, raffleBox: Json): Unit = try{
    val address = raffleBox.hcursor.downField("address").as[String].getOrElse(throw parseException())
    val state = raffleStateByAddress(address)
    val raffle = Raffle(raffleBox)
    if (state != savedRaffle.state) raffleCacheDAO.updateStateById(savedRaffle.id, state)
    if (state == active.id) {
      if(raffle.tickets != savedRaffle.tickets) {
        val participants: Long = raffleUtils.raffleParticipants(raffle.tokenId)
        val lastActivity: Long = raffleBox.hcursor.downField("settlementHeight").as[Long].getOrElse(0)
        raffleCacheDAO.updateActivity(savedRaffle.id, raffle.raised, raffle.tickets, participants, lastActivity)
        activeRaffleTxUpdate(raffle.tokenId)
      }
      if(client.getHeight > raffle.deadlineHeight && raffle.raised >= raffle.goal)
        raffleCacheDAO.updateStateById(savedRaffle.id, succeed.id)
      else if(client.getHeight > raffle.deadlineHeight && raffle.raised < raffle.goal)
        raffleCacheDAO.updateStateById(savedRaffle.id, failed.id)
    }
    if (state == failed.id && savedRaffle.tickets - savedRaffle.redeemedTickets != raffle.tickets) {
      UnsuccessfulRaffleTxUpdate(raffle.tokenId)
      raffleCacheDAO.updateRedeemed(savedRaffle.id, savedRaffle.tickets - raffle.tickets)
    }
  } catch{
    case e: parseException => logger.warn(e.getMessage)
    case e: Throwable => logger.error(utils.getStackTraceStr(e))
  }

  def raffleSearch(): Unit = {
    try {
      var offset: Int = 0
      var items = raffleUtils.getAllRaffleBoxes(offset)
      raffleCacheDAO.updatingStatus()

      while (items != null && items.nonEmpty) {
        items.foreach(box => {
          val raffle: Raffle = Raffle(box)
          if(Configs.exceptionList.contains(raffle.tokenId)){
            try { raffleCacheDAO.deleteByTokenId(raffle.tokenId)}
            catch {
              case _: Throwable => logger.debug("Excepted raffle doesn't exist in database" + raffle.tokenId)
            }
          }else {
            val address = box.hcursor.downField("address").as[String].getOrElse(throw parseException())
            val state = raffleStateByAddress(address)
            try {
              val savedRaffle = raffleCacheDAO.byTokenId(raffle.tokenId)
              raffleCacheDAO.acceptUpdating(savedRaffle.id)
              updateRaffle(savedRaffle, box)
              logger.debug(s"raffle with id ${raffle.tokenId} had been updated so far")
            }
            catch {
              case _: Throwable =>
                logger.debug("New raffle found with Token Id: " + raffle.tokenId)
                val participants = raffleUtils.raffleParticipants(raffle.tokenId)
                // TODO change the timestamp
                val lastActivity: Long = box.hcursor.downField("settlementHeight").as[Long].getOrElse(throw parseException())
                raffleCacheDAO.insert(raffle, participants, state, lastActivity, lastActivity)
            }
          }
        })
        offset += 100
        items = raffleUtils.getAllRaffleBoxes(offset)
      }

      raffleCacheDAO.selectAfterUpdating().foreach(raffle => {
        if (raffle.state == succeed.id) {
          try {
            txCacheDAO.winnerByTokenId(raffle.tokenId)
            raffleCacheDAO.completeByTokenId(raffle.tokenId)
          }
          catch {
            case _: Throwable => SuccessfulRaffleTxUpdate(raffle.tokenId)
          }
        }
        else if (raffle.state == failed.id) {
          if (txCacheDAO.refundedTickets(raffle.tokenId) >= raffle.tickets) raffleCacheDAO.completeByTokenId(raffle.tokenId)
          else UnsuccessfulRaffleTxUpdate(raffle.tokenId)
        }
        else {
          if (raffle.deadlineHeight > client.getHeight)
            logger.warn(s"uncompleted raffle with token ${raffle.tokenId} not founded in the network")
          else {
            if (raffle.raised >= raffle.goal) raffleCacheDAO.updateStateById(raffle.id, succeed.id)
            else raffleCacheDAO.updateStateById(raffle.id, failed.id)
          }
        }
      })
    }
    catch{
      case e: noRaffleException => logger.warn(e.getMessage)
      case _: internalException =>
      case _: connectionException =>
      case e: ErgoClientException => logger.warn(e.getMessage)
      case e: parseException => logger.error(e.getMessage)
      case e: Throwable => logger.error(utils.getStackTraceStr(e))
    }
  }

  def activeRaffleTxUpdate(tokenId: String): Unit = try {
    // Tickets
    var offset = 0
    var tickets = raffleUtils.getTicketBoxes(tokenId, offset)
    while (tickets != null && tickets.nonEmpty) {
      tickets.foreach(ticketBox => {
        val ticket = Ticket(ticketBox)
        try txCacheDAO.byTxId(ticket.txId)
        catch {case _: Throwable => txCacheDAO.insert(ticket.txId, tokenId, ticket.tokenCount, txType.ticket.id, ticket.walletAddress, Configs.noTx) }
      })
      offset += 100
      tickets = raffleUtils.getTicketBoxes(tokenId, offset)
    }
  } catch{
    case _: internalException =>
    case _: connectionException =>
    case e: Throwable => logger.error(utils.getStackTraceStr(e))
  }

  def SuccessfulRaffleTxUpdate(tokenId: String): Unit = try {
    val winnerRaffleBox = raffleUtils.getWinnerBox(tokenId)
    if(winnerRaffleBox != null){
      val charityTx: String = winnerRaffleBox.hcursor.downField("transactionId").as[String].getOrElse(throw parseException())
      val raffleInfo = Raffle(winnerRaffleBox)
      // Adding Charity Transaction
      txCacheDAO.insert(charityTx, tokenId, tokenCount = 0, txType.charity.id, raffleInfo.charityAddr, Configs.noTx)
      val spendTxId: String = winnerRaffleBox.hcursor.downField("spentTransactionId").as[String].getOrElse(Configs.noTx)
      if (spendTxId != Configs.noTx) {
        var offset = 0
        var tickets = raffleUtils.getTicketBoxes(tokenId, offset)
        while (tickets != null && tickets.nonEmpty) {
          tickets.foreach(ticketBox => {
            val ticket = Ticket(ticketBox)
            // Updating Tickets
            try txCacheDAO.byTxId(ticket.txId)
            catch {case _: Throwable => txCacheDAO.insert(ticket.txId, tokenId, ticket.tokenCount, txType.ticket.id, ticket.walletAddress, Configs.noTx) }
            val ticketSpendTxId: String = ticketBox.hcursor.downField("spentTransactionId").as[String].getOrElse(Configs.noTx)
            if (spendTxId == ticketSpendTxId) {
              // Adding Winner Transaction
              txCacheDAO.insert(ticketSpendTxId, tokenId, ticket.tokenCount, txType.winner.id, ticket.walletAddress, Configs.noTx)
            }
          })
          offset += 100
          tickets = raffleUtils.getTicketBoxes(tokenId, offset)
        }
      }
    }
  } catch{
    case _: internalException =>
    case _: connectionException =>
    case e: parseException => logger.error(e.getMessage)
    case e: Throwable => logger.error(utils.getStackTraceStr(e))
  }

  def UnsuccessfulRaffleTxUpdate(tokenId: String): Unit = try{
    // Ticket Refund Txs
    var offset = 0
    var tickets = raffleUtils.getTicketBoxes(tokenId, offset)
    while (tickets != null && tickets.nonEmpty) {
      tickets.foreach(ticketBox => {
        val ticket = Ticket(ticketBox)
        val spendTxId: String = ticketBox.hcursor.downField("spentTransactionId").as[String].getOrElse(Configs.noTx)
        // Ticket Tx
        try {
          val txCache = txCacheDAO.byTxId(ticket.txId)
          if(txCache.spendTx == Configs.noTx && spendTxId != Configs.noTx) txCacheDAO.updateSpendTx(txCache.id, spendTxId)
        }
        catch {
          case _: Throwable =>
            txCacheDAO.insert(ticket.txId, tokenId, ticket.tokenCount, txType.ticket.id, ticket.walletAddress, spendTxId)
        }
      })
      offset += 100
      tickets = raffleUtils.getTicketBoxes(tokenId, offset)
    }
  } catch{
    case _: connectionException =>
    case _: internalException =>
    case e: Throwable => logger.error(utils.getStackTraceStr(e))
  }

  def raffleInitialSearch(): Unit ={
    logger.debug("Updating Raffle cache started")
    try {
      var raffleList: List[Raffle] = List()
      var offset: Int = 0
      var items: List[Json] = null
      do {
        val response = explorer.getAllTokenBoxes(Configs.token.service, offset, 100)
        offset += 100
        try {
          items = response.hcursor.downField("items").as[List[Json]].getOrElse(throw new parseException)
            .filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null).size > 1)
            .filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null).head
              .hcursor.downField("tokenId").as[String].getOrElse("") == Configs.token.service)
          raffleList = raffleList ::: items.map(item => Raffle(item))
        } catch {
          case _: parseException =>
          case e: Throwable => logger.error(utils.getStackTraceStr(e))
        }
      } while (items != null && items.nonEmpty)
      val maxParticipation: Map[String, (Long, Long)] = raffleList.map(r => (r.tokenId, r.raised, r.tickets))
        .groupBy(_._1).mapValues(seq => (seq.map(_._2).max, seq.map(_._3).max))

      var raffleIdList: List[String] = try{
        raffleCacheDAO.all.map(_.tokenId).toList
      } catch {
        case _: Throwable => List[String]()
      }

      logger.info(s"Found ${raffleList.size} raffle boxes belonging to ${maxParticipation.size} number of raffles")
      raffleList.foreach(raffle => {
        if(!Configs.exceptionList.contains(raffle.tokenId))
          if(!raffleIdList.contains(raffle.tokenId)) {
            logger.info("New raffle found with Token Id: " + raffle.tokenId)
            // TODO change the creationTime
            val participants: Long = raffleUtils.raffleParticipants(raffle.tokenId)
            raffleCacheDAO.initialInsert(raffle, participants, 0,0,
              maxParticipation(raffle.tokenId)._1, maxParticipation(raffle.tokenId)._2)
            raffleIdList = raffleIdList :+ raffle.tokenId
          }
      })
    } catch{
      case _: connectionException =>
      case e: parseException => logger.error(e.getMessage)
      case e: Throwable => logger.error(utils.getStackTraceStr(e))
    }
  }
}
