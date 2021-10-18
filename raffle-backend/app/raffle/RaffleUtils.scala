package raffle


import dao.{RaffleCacheDAO, TxCacheDAO}
import helpers.{Configs, Utils, connectionException, explorerException, failedTxException, internalException, parseException}
import io.circe.Json
import io.circe.parser.parse
import javax.inject.Inject
import models.{Raffle, Ticket}
import network.{Client, Explorer}
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.appkit.{Address, ErgoToken, ErgoValue, InputBox}
import play.api.Logger

import scala.collection.JavaConverters._
import scala.collection.mutable.{ListBuffer, Seq}

class RaffleUtils @Inject()(client: Client, explorer: Explorer, addresses: Addresses, utils: Utils,
                            raffleCacheDAO: RaffleCacheDAO, raffleCacheUtils: RaffleCacheUtils, txCacheDAO: TxCacheDAO) {

  private val logger: Logger = Logger(this.getClass)

  def rafflesWithSorting(sorting: String, states: List[String], offset: Int, limit: Int): Json ={
    try {
      val allRaffles = raffleCacheDAO.all.filter(raffle => states.contains(raffle.state))
      val sortedRaffles = {
        if(sorting == "createTime") allRaffles.sortBy(_.creationTime)(Ordering[Long].reverse)
        else if(sorting == "deadline") allRaffles.sortBy(_.deadlineHeight)
        else allRaffles.sortBy(_.lastActivity)(Ordering[Long].reverse)
      }
      val end = Math.min(limit + offset, sortedRaffles.size)
      var raffles: ListBuffer[Json] = ListBuffer()
      var raffleCount: Int = 0
      for (i <- offset until end) {
        raffleCount += 1
        val raffle = sortedRaffles(i)
        raffles += Json.fromFields(List(
          ("id", Json.fromString(raffle.tokenId)),
          ("name", Json.fromString(raffle.name)),
          ("description", Json.fromString(raffle.description)),
          ("deadline", Json.fromLong(raffle.deadlineHeight)),
          ("picture", parse(raffle.picLinks).getOrElse(Json.fromValues(List[Json]()))),
          ("erg", Json.fromLong(raffle.raised)),
          ("goal", Json.fromLong(raffle.goal)),
          ("status", Json.fromString(raffle.state)),
          ("donatedPeople", Json.fromLong(raffle.participants))
        ))
      }

      Json.fromFields(List(
        ("items", Json.fromValues(raffles.toList)),
        ("total", Json.fromInt(raffleCount)),
      ))
    }
    catch {
      case e: Throwable => {
        logger.error(utils.getStackTraceStr(e))
        throw new internalException
      }
    }
  }

  def userTickets(raffleId: String, wallerAdd: String): Json ={
    try {
      var C = 0
      var total = 100
      var selectedTickets: ListBuffer[Json] = ListBuffer()
      var totalTickets: Long = 0
      var totalRecords: Long = 0
      do {
        val response = explorer.getUnspentTokenBoxes(raffleId, C, 100)
        val tickets: Seq[Json] = response.hcursor.downField("items").as[Seq[Json]].getOrElse(throw parseException())
          .filter(_.hcursor.downField("assets").as[List[Json]].getOrElse(null).size == 1)
        total = response.hcursor.downField("total").as[Int].getOrElse(0)
        for (ticketBox <- tickets) {
          val ticket = Ticket(ticketBox)
          if (ticket.walletAddress == wallerAdd) {
            selectedTickets += Json.fromFields(List(
              ("id", Json.fromString(ticket.txId)),
              ("tickets", Json.fromLong(ticket.tokenCount)),
              ("link", Json.fromString(utils.getTransactionFrontLink(ticket.txId)))
            ))
            totalTickets += ticket.tokenCount
            totalRecords += 1
          }
        }
        C += 100
      } while (C < total)
      Json.fromFields(List(
        ("items", Json.fromValues(selectedTickets.toList)),
        ("totalTickets", Json.fromLong(totalTickets)),
        ("total", Json.fromLong(totalRecords))
      ))
    } catch {
      case _: parseException => throw connectionException()
      case e: connectionException => {
        logger.warn(e.getMessage)
        throw e
      }
      case e: Throwable => {
        logger.error(utils.getStackTraceStr(e))
        throw new internalException
      }
    }
  }

  def raffleByTokenId(tokenId: String): Json={
    try {
      var boxes: Json = null
      var raffleBox: Json = null
      var offset = 0

      while (raffleBox == null) {
        boxes = explorer.getUnspentTokenBoxes(Configs.token.service, offset, 100)
        raffleBox = boxes.hcursor.downField("items").as[Seq[Json]].getOrElse(throw parseException())
          .filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null)(1)
          .hcursor.downField("tokenId").as[String].getOrElse("") == tokenId).head
        offset += 100
      }

      val raffle = Raffle(raffleBox)
      val savedRaffle = raffleCacheDAO.byTokenId(tokenId)
      raffleCacheDAO.updateRaised(savedRaffle.id, raffle.raised, raffle.tickets)

      Json.fromFields(List(
        ("id", Json.fromString(raffle.tokenId)),
        ("name", Json.fromString(raffle.name)),
        ("description", Json.fromString(raffle.description)),
        ("deadline", Json.fromLong(raffle.deadlineHeight)),
        ("goal", Json.fromLong(raffle.goal)),
        ("picture", parse(raffle.picLinks).getOrElse(Json.fromValues(List[Json]()))),
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
        ("status", Json.fromString(savedRaffle.state)),
        ("txFee", Json.fromLong(Configs.fee))
      ))
    } catch {
      case e: explorerException => {
        logger.warn(e.getMessage)
        throw connectionException()
      }
      case _: parseException => throw connectionException()
      case _: java.util.NoSuchElementException => throw new Throwable("No Raffle exist with this id")
      case e: Throwable => {
        logger.error(utils.getStackTraceStr(e))
        throw new internalException
      }
    }
  }

  def raffleTxsByTokenId(tokenId: String, offset: Int, limit: Int): Json ={
    try {
      var transactions: ListBuffer[Json] = ListBuffer()
      var txCount: Int = 0
      val txs = txCacheDAO.byTokenId(tokenId)
      val end = Math.min(txs.size, offset + limit)

      for (i <- offset until end) {
        val tx = txs(i)
        txCount += 1
        transactions += Json.fromFields(List(
          ("id", Json.fromString(tx.txId)),
          ("address", Json.fromString(tx.wallerAdd)),
          ("type", Json.fromString(tx.txType)),
          ("tickets", Json.fromLong(tx.tokenCount)),
          ("link", Json.fromString(utils.getTransactionFrontLink(tx.txId)))
        ))
      }
      Json.fromFields(List(
        ("items", Json.fromValues(transactions.toList)),
        ("total", Json.fromInt(txCount))
      ))
    } catch {
      case _: Throwable => throw new Throwable("This raffle doesn't exist or not finished yet, no transaction found")
    }
  }

  def walletDonations(walletAdd: String, offset: Int, limit: Int): Json = {
    try {
      var donations: ListBuffer[Json] = ListBuffer()
      var totalRecords: Long = 0
      val tickets: List[(String, Long)] = txCacheDAO.byWalletAdd(walletAdd).map(t => (t.tokenId, t.tokenCount))
        .groupBy(_._1).mapValues(seq => seq.map(_._2).sum).toList
      val end = Math.min(tickets.size, offset + limit)

      for (i <- offset until end) {
        val ticket = tickets(i)
        val raffle = raffleCacheDAO.byTokenId(ticket._1)
        donations += Json.fromFields(List(
          ("id", Json.fromString(raffle.tokenId)),
          ("name", Json.fromString(raffle.name)),
          ("description", Json.fromString(raffle.description)),
          ("deadline", Json.fromLong(raffle.deadlineHeight)),
          ("picture", parse(raffle.picLinks).getOrElse(Json.fromValues(List[Json]()))),
          ("erg", Json.fromLong(raffle.raised)),
          ("goal", Json.fromLong(raffle.goal)),
          ("tickets", Json.fromLong(ticket._2)),
        ))
        totalRecords += 1
      }

      Json.fromFields(List(
        ("items", Json.fromValues(donations.toList)),
        ("total", Json.fromLong(totalRecords))
      ))
    } catch {
      case _: parseException => throw connectionException()
      case e: connectionException => {
        logger.warn(e.getMessage)
        throw e
      }
      case e: Throwable => {
        logger.error(utils.getStackTraceStr(e))
        throw new internalException
      }
    }
  }

  def walletWins(walletAdd: String, offset: Int, limit: Int): Json = {
    try{
      var wins: ListBuffer[Json] = ListBuffer()
      val tickets = txCacheDAO.winnerByWalletAdd(walletAdd)
      val end = Math.min(tickets.size, offset + limit)
      var totalRecords: Long = 0

      for (i <- offset until end) {
        val ticket = tickets(i)
        val raffle = raffleCacheDAO.byTokenId(ticket.tokenId)
        wins += Json.fromFields(List(
          ("id", Json.fromString(raffle.tokenId)),
          ("name", Json.fromString(raffle.name)),
          ("description", Json.fromString(raffle.description)),
          ("deadline", Json.fromLong(raffle.deadlineHeight)),
          ("picture", parse(raffle.picLinks).getOrElse(Json.fromValues(List[Json]()))),
          ("erg", Json.fromLong(raffle.raised)),
          ("goal", Json.fromLong(raffle.goal)),
          ("tickets", Json.fromLong(ticket.tokenCount)),
          ("link", Json.fromString(utils.getTransactionFrontLink(ticket.txId)))
        ))
        totalRecords += 1
      }

      Json.fromFields(List(
        ("items", Json.fromValues(wins.toList)),
        ("total", Json.fromLong(totalRecords))
      ))
    } catch{
      case _: Throwable => throw new Throwable("No winner tickets found")
    }
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
      case e:failedTxException => {
        logger.warn(e.getMessage)
        throw failedTxException()
      }
      case e:Throwable =>
        logger.error(utils.getStackTraceStr(e))
        throw new Throwable("Something is wrong on refunding")
    }
  }

}
