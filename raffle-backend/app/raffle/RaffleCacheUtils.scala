package raffle

import java.time.LocalDateTime

import dao.{RaffleCacheDAO, TxCacheDAO}
import helpers.{Configs, Utils, connectionException, noRaffleException, parseException}
import javax.inject.Inject
import network.{Client, Explorer}
import play.api.Logger
import io.circe.Json
import models.{Raffle, Ticket}


class RaffleCacheUtils @Inject()(client: Client, explorer: Explorer, utils: Utils, addresses: Addresses,
                                 raffleCacheDAO: RaffleCacheDAO, txCacheDAO: TxCacheDAO) {
  private val logger: Logger = Logger(this.getClass)

  def raffleStateByAddress(address: String): String ={
    if(address == addresses.raffleInactiveAddress.toString) "inactive"
    else if(address == addresses.raffleActiveAddress.toString) "active"
    else if(address == addresses.raffleRedeemAddress.toString) "unsuccessful"
    else "other"
  }

  def raffleSearch(): Unit = {
    logger.debug("Searching for new raffles started")
    try {
      var items: List[Json] = null
      var response = explorer.getUnspentTokenBoxes(Configs.token.service, 0, 100)
      try {
        items = response.hcursor.downField("items").as[List[Json]].getOrElse(null)
          .filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null).size > 1)
          .filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null).head
            .hcursor.downField("tokenId").as[String].getOrElse("") == Configs.token.service)
      } catch {
        case _: Throwable => throw new noRaffleException
      }
      raffleCacheDAO.updatingStatus()

      var offset: Int = 100
      while (items != null && items.nonEmpty) {
        items.foreach(box => {
          val raffle: Raffle = Raffle(box)
          val address = box.hcursor.downField("address").as[String].getOrElse("")
          val state = raffleStateByAddress(address)
          try {
            val savedRaffle = raffleCacheDAO.byTokenId(raffle.tokenId)
            raffleCacheDAO.acceptUpdating(savedRaffle.id)
            if (state != savedRaffle.state) raffleCacheDAO.updateStateById(savedRaffle.id, state)
            if (state == "active" && raffle.tickets != savedRaffle.tickets) {
              val participants: Long = utils.raffleParticipants(raffle.tokenId)
              val lastActivity: Long = box.hcursor.downField("settlementHeight").as[Long].getOrElse(0)
              raffleCacheDAO.updateRaised(savedRaffle.id, raffle.raised, raffle.tickets, participants, lastActivity)
            }
            if (state == "unsuccessful" && savedRaffle.tickets - savedRaffle.redeemedTickets != raffle.tickets) {
              UnsuccessfulRaffleTxUpdate(raffle.tokenId)
              raffleCacheDAO.updateRedeemed(savedRaffle.id, raffle.tickets - savedRaffle.tickets)
            }
            logger.debug(s"raffle with id ${raffle.tokenId} had been updated so far")
          }
          catch {
            case e: Throwable => {
              logger.debug("New raffle found with Token Id: " + raffle.tokenId)
              val participants = utils.raffleParticipants(raffle.tokenId)
              // TODO change the timestamp
              val lastActivity: Long = box.hcursor.downField("settlementHeight").as[Long].getOrElse(0)
              raffleCacheDAO.insert(raffle, participants, state, lastActivity, lastActivity)
            }
          }
        })
        response = explorer.getUnspentTokenBoxes(Configs.token.service, offset, 100)
        try {
          items = response.hcursor.downField("items").as[List[Json]].getOrElse(null)
            .filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null).size > 1)
            .filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null).head
              .hcursor.downField("tokenId").as[String].getOrElse("") == Configs.token.service)
        } catch {
          case _: Throwable => throw new noRaffleException
        }
        offset += 100
      }

      raffleCacheDAO.selectAfterUpdating().foreach(raffle => {
        if (raffle.state == "successful") {
          try {
            txCacheDAO.winnerByTokenId(raffle.tokenId)
            raffleCacheDAO.completeByTokenId(raffle.tokenId)
          }
          catch {
            case _: Throwable => SuccessfulRaffleTxUpdate(raffle.tokenId)
          }
        }
        else if (raffle.state == "unsuccessful") {
          if (raffle.tickets == raffle.redeemedTickets) raffleCacheDAO.completeByTokenId(raffle.tokenId)
          else UnsuccessfulRaffleTxUpdate(raffle.tokenId)
        }
        else {
          if (raffle.deadlineHeight > client.getHeight)
            logger.warn(s"uncompleted raffle with token ${raffle.tokenId} not founded in the network")
          else {
            if (raffle.raised >= raffle.goal) raffleCacheDAO.updateStateById(raffle.id, "successful")
            else raffleCacheDAO.updateStateById(raffle.id, "unsuccessful")
          }
        }
      })
    }
    catch{
      case e: noRaffleException => logger.warn(e.getMessage)
      case e: connectionException => logger.info(e.getMessage)
      case _: parseException =>
      case e: Throwable => logger.error(utils.getStackTraceStr(e))
    }
    logger.info("Updating raffles finished")
  }

  def SuccessfulRaffleTxUpdate(tokenId: String): Unit ={
    // Tickets
    val tickets = explorer.getBoxesByErgoTree(addresses.getTicketContract().getErgoTree, tokenId)
        .hcursor.downField("items").as[Seq[Json]].getOrElse(null)
    tickets.foreach(ticketBox =>{
      val ticket = Ticket(ticketBox)
      try{
        txCacheDAO.byTxId(ticket.txId)
      }
      catch{
        case _: Throwable => txCacheDAO.insert(ticket.txId, tokenId, ticket.tokenCount, "Ticket", ticket.walletAddress)
      }
      // Winner Reward
      val spendTxId: String = ticketBox.hcursor.downField("spentTransactionId").as[String].getOrElse("")
      if(spendTxId != "")
        txCacheDAO.insert(spendTxId, tokenId, ticket.tokenCount, "Winner", ticket.walletAddress)
    })
  }

  def UnsuccessfulRaffleTxUpdate(tokenId: String): Unit ={
    // Ticket Refund Txs
    val tickets = explorer.getBoxesByErgoTree(addresses.getTicketContract().getErgoTree, tokenId)
      .hcursor.downField("items").as[Seq[Json]].getOrElse(null)
    tickets.foreach(ticketBox =>{
      val ticket = Ticket(ticketBox)
      try{
        txCacheDAO.byTxId(ticket.txId)
      }
      catch{
        case _: Throwable => {
          val spendTxId: String = ticketBox.hcursor.downField("spentTransactionId").as[String].getOrElse("")
          if(spendTxId != "")
            txCacheDAO.insert(spendTxId, tokenId, ticket.tokenCount, "Refund", ticket.walletAddress)
        }
      }
    })
  }
}
