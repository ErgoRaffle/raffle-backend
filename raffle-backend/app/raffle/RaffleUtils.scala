package raffle

import java.nio.charset.StandardCharsets

import dao.RaffleCacheDAO
import helpers.{Configs, Utils, connectionException, explorerException, failedTxException, internalException, parseException}
import io.circe.Json
import io.circe.parser.parse
import javax.inject.Inject
import network.{Client, Explorer}
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.appkit.{Address, ErgoToken, ErgoValue, InputBox}
import play.api.Logger
import sigmastate.serialization.ErgoTreeSerializer
import special.collection.Coll

import scala.collection.JavaConverters._
import scala.collection.mutable.{ListBuffer, Seq}

class RaffleUtils @Inject()(client: Client, explorer: Explorer, addresses: Addresses, utils: Utils,
                            raffleCacheDAO: RaffleCacheDAO) {

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
          ("goal", Json.fromLong(raffle.goal))
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
        for (ticket <- tickets) {
          val registers = ticket.hcursor.downField("additionalRegisters").as[Json].getOrElse(null)
          val participantAddressByte: Array[Byte] = ErgoValue.fromHex(registers.hcursor.downField("R4").as[Json].getOrElse(null)
            .hcursor.downField("serializedValue").as[String].getOrElse(""))
            .getValue.asInstanceOf[Coll[Byte]].toArray
          val participantAddress = Configs.addressEncoder.fromProposition(ErgoTreeSerializer.DefaultSerializer
            .deserializeErgoTree(participantAddressByte)).get.toString
          if (participantAddress == wallerAdd) {
            val txId = ticket.hcursor.downField("transactionId").as[String].getOrElse("")
            val count: Long = ticket.hcursor.downField("assets").as[List[Json]].getOrElse(null).head
              .hcursor.downField("amount").as[Long].getOrElse(0)
            selectedTickets += Json.fromFields(List(
              ("txId", Json.fromString(txId)),
              ("count", Json.fromLong(count))
            ))
            totalTickets += count
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
        throw new Throwable("Error occurred during responding the request")
      }
    }
  }

  def raffleByTokenId(tokenId: String): Json={
    try {
      var boxes = explorer.getUnspentTokenBoxes(Configs.token.service, 0, 100)
      var items = boxes.hcursor.downField("items").as[Seq[Json]].getOrElse(throw parseException())
        .filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null).size > 1)
        .filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null).head
          .hcursor.downField("tokenId").as[String].getOrElse("") == Configs.token.service)

      var c: Int = 1
      var item: Json = items.filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null)(1)
        .hcursor.downField("tokenId").as[String].getOrElse("") == tokenId).head

      while (item == null) {
        boxes = explorer.getUnspentTokenBoxes(Configs.token.service, c * 100, 100)
        items = boxes.hcursor.downField("items").as[Seq[Json]].getOrElse(throw parseException())
          .filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null).head
            .hcursor.downField("tokenId").as[String].getOrElse("")
            == Configs.token.service)
        c += 1
        item = items.filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null).head
          .hcursor.downField("tokenId").as[String].getOrElse("") == tokenId).head
      }

      val id = item.hcursor.downField("assets").as[Seq[Json]].getOrElse(null)(1)
        .hcursor.downField("tokenId").as[String].getOrElse("")
      val registers = item.hcursor.downField("additionalRegisters").as[Json].getOrElse(null)
      val R4: Array[Long] = ErgoValue.fromHex(registers.hcursor.downField("R4").as[Json].getOrElse(null)
        .hcursor.downField("serializedValue").as[String].getOrElse(""))
        .getValue.asInstanceOf[Coll[Long]].toArray
      val charityPercent = R4(0)
      val serviceFee = R4(1)
      val winnerPercent = 100 - charityPercent - serviceFee
      val ticketPrice = R4(2)
      val goal = R4(3)
      val deadlineHeight = R4(4)
      val totalSoldTicket = R4(5)
      val totalRaised = totalSoldTicket * ticketPrice

      val strListByte: Array[Coll[Byte]] = ErgoValue.fromHex(registers.hcursor.downField("R6").as[Json].getOrElse(null)
        .hcursor.downField("serializedValue").as[String].getOrElse(""))
        .getValue.asInstanceOf[Coll[Coll[Byte]]].toArray
      val name: String = new String(strListByte(0).toArray, StandardCharsets.UTF_8)
      val description: String = new String(strListByte(1).toArray, StandardCharsets.UTF_8)

      val charityAddressByte: Array[Byte] = ErgoValue.fromHex(registers.hcursor.downField("R5").as[Json].getOrElse(null)
        .hcursor.downField("serializedValue").as[String].getOrElse(""))
        .getValue.asInstanceOf[Coll[Byte]].toArray
      val charityAddress = Configs.addressEncoder.fromProposition(ErgoTreeSerializer.DefaultSerializer
        .deserializeErgoTree(charityAddressByte)).get.toString
      val currentHeight: Long = client.getHeight
      val fee = Configs.fee

      Json.fromFields(List(
        ("id", Json.fromString(id)),
        ("name", Json.fromString(name)),
        ("description", Json.fromString(description)),
        ("deadline", Json.fromLong(deadlineHeight)),
        ("erg", Json.fromLong(totalRaised)),
        ("charityAddr", Json.fromString(charityAddress)),
        ("winnerPercent", Json.fromLong(winnerPercent)),
        ("charityPercent", Json.fromLong(charityPercent)),
        ("min", Json.fromLong(goal)),
        ("ticketPrice", Json.fromLong(ticketPrice)),
        ("fee", Json.fromLong(fee)),
        ("currentHeight", Json.fromLong(currentHeight))
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
        throw new Throwable("Something is wrong")
      }
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
