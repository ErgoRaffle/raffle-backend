package raffle


import dao.{RaffleCacheDAO, TxCacheDAO}
import helpers.{Configs, Utils, connectionException, explorerException, failedTxException, internalException, parseException}
import io.circe.Json
import io.circe.parser.parse
import javax.inject.Inject
import models.{Raffle, Ticket, TxCache}
import network.{Client, Explorer}
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.appkit.{Address, ErgoToken, ErgoValue, InputBox}
import play.api.Logger

import scala.collection.JavaConverters._
import scala.collection.mutable.{ListBuffer, Seq}

class RaffleUtils @Inject()(client: Client, explorer: Explorer, addresses: Addresses, utils: Utils,
                            raffleCacheDAO: RaffleCacheDAO, raffleCacheUtils: RaffleCacheUtils, txCacheDAO: TxCacheDAO) {

  private val logger: Logger = Logger(this.getClass)

  def rafflesWithSorting(sorting: String, state: String, offset: Int, limit: Int): Json ={
    try {
      val raffles = raffleCacheDAO.selectRaffles(state, sorting, offset, limit)
      val serializedRaffles = raffles._1.map(raffle => {
        Json.fromFields(List(
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
      })

      Json.fromFields(List(
        ("items", Json.fromValues(serializedRaffles.toList)),
        ("total", Json.fromInt(raffles._2)),
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
    } catch {
      case e: Throwable => throw new Throwable(e.getMessage)
    }
  }

  def walletDonations(walletAdd: String, offset: Int, limit: Int): Json = {
    try {
      val queryResult = txCacheDAO.byWalletAdd(walletAdd, offset, limit)
      val tickets: List[(String, Long)] = queryResult._1.map(ticket => (ticket._1, ticket._2.getOrElse(0).asInstanceOf[Long])).toList
      val totalRecords: Long = queryResult._2

      val donations = tickets.map(ticket => {
        val raffle = raffleCacheDAO.byTokenId(ticket._1)
        Json.fromFields(List(
          ("id", Json.fromString(raffle.tokenId)),
          ("name", Json.fromString(raffle.name)),
          ("description", Json.fromString(raffle.description)),
          ("deadline", Json.fromLong(raffle.deadlineHeight)),
          ("picture", parse(raffle.picLinks).getOrElse(Json.fromValues(List[Json]()))),
          ("erg", Json.fromLong(raffle.raised)),
          ("goal", Json.fromLong(raffle.goal)),
          ("tickets", Json.fromLong(ticket._2.toLong)),
        ))
      })

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
      val queryResult = txCacheDAO.winnerByWalletAdd(walletAdd, offset, limit)
      val tickets = queryResult._1
      val totalRecords: Long = queryResult._2

      val wins = tickets.map(ticket => {
        val raffle = raffleCacheDAO.byTokenId(ticket.tokenId)
        Json.fromFields(List(
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
      })

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
