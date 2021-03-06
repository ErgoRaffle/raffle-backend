package models

import java.nio.charset.StandardCharsets
import helpers.{Configs, Utils, parseException}
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.appkit.{ErgoValue, InputBox}
import sigmastate.serialization.ErgoTreeSerializer
import special.collection.{Coll, CollOverArray}

import scala.collection.mutable.Seq


case class CreateReq(id: Long, name: String, description: String, goal: Long, deadlineHeight: Long, charityPercent: Int,
                     charityAddr: String, ticketPrice: Long, state: Int, walletAddress: String, paymentAddress: String,
                     createTxId: Option[String],  mergeTxId: Option[String], picLinks: String, timeStamp: String,
                     ttl: Long, deleted: Boolean)


case class DonateReq(id: Long, ticketCount: Long, fee: Long, raffleDeadline: Long , state: Int, paymentAddress: String,
                     raffleToken: String, donateTxID: Option[String], participantAddress: String,
                     timeStamp: String, ttl: Long, deleted: Boolean)

case class RaffleCache(id: Long, name: String, description: String, goal: Long, raised: Long,
                       deadlineHeight: Long, serviceFee: Int, charityPercent: Int, charityAddr: String,
                       ticketPrice: Long, picLinks: String, tickets: Long, participants: Long, redeemedTickets: Long,
                       state: Int, tokenId: String, creationTime: Long, lastActivity: Long, isUpdating: Boolean,
                       completed: Boolean)

case class TxCache(id: Long, txId: String, tokenId: String, tokenCount: Long, txType: Int, wallerAdd: String, spendTx: String)

case class Raffle(name: String, description: String, goal: Long, raised: Long, deadlineHeight: Long,
                  serviceFee: Int, charityPercent: Int, charityAddr: String, winnerPercent: Int, ticketPrice: Long,
                  tickets: Long, picLinks: String, tokenId: String)

object Raffle{
  def apply(raffleBox: Json): Raffle = {
    try {
      val tokenId = raffleBox.hcursor.downField("assets").as[Seq[Json]].getOrElse(null)(1)
        .hcursor.downField("tokenId").as[String].getOrElse("")
      val registers = raffleBox.hcursor.downField("additionalRegisters").as[Json].getOrElse(null)
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

      val charityAddressByte: Array[Byte] = ErgoValue.fromHex(registers.hcursor.downField("R5").as[Json].getOrElse(null)
        .hcursor.downField("serializedValue").as[String].getOrElse(""))
        .getValue.asInstanceOf[Coll[Byte]].toArray
      val charityAddress = Configs.addressEncoder.fromProposition(ErgoTreeSerializer.DefaultSerializer
        .deserializeErgoTree(charityAddressByte)).get.toString
      val strListByte: Array[Coll[Byte]] = ErgoValue.fromHex(registers.hcursor.downField("R6").as[Json].getOrElse(null)
        .hcursor.downField("serializedValue").as[String].getOrElse(""))
        .getValue.asInstanceOf[Coll[Coll[Byte]]].toArray
      val name: String = new String(strListByte(0).toArray, StandardCharsets.UTF_8)
      val description: String = new String(strListByte(1).toArray, StandardCharsets.UTF_8)
      val picLinks: String = {
        var Links: List[String] = List()
        for(i <- 2 until strListByte.size){
          var url = new String(strListByte(i).toArray, StandardCharsets.UTF_8)
          if(url.startsWith(Configs.ipfsPrefix)) url = url.replace(Configs.ipfsPrefix, Configs.ipfsResolver)
          if(url.contains("://")) Links = Links :+ url
        }
        Links.asJson.toString()
      }

      new Raffle(name, description, goal, totalRaised, deadlineHeight, serviceFee.toInt, charityPercent.toInt,
        charityAddress, winnerPercent.toInt, ticketPrice, totalSoldTicket, picLinks, tokenId)
    }
    catch{
      case _: Throwable => throw new parseException
    }
  }
  def apply(raffleCache: RaffleCache): Raffle ={
    val winnerPercent = 100 - raffleCache.charityPercent - raffleCache.serviceFee
    new Raffle(raffleCache.name, raffleCache.description, raffleCache.goal, raffleCache.raised, raffleCache.deadlineHeight,
      raffleCache.serviceFee, raffleCache.charityPercent, raffleCache.charityAddr, winnerPercent, raffleCache.ticketPrice,
      raffleCache.tickets, raffleCache.picLinks, raffleCache.tokenId)
  }

  def apply(raffleBox: InputBox, utils: Utils): Raffle ={
    val r4 = raffleBox.getRegisters.get(0).getValue.asInstanceOf[CollOverArray[Long]].toArray
    val charityPercent = r4(0)
    val serviceFee = r4(1)
    val ticketPrice = r4(2)
    val goal = r4(3)
    val deadlineHeight = r4(4)
    val totalSoldTicket = r4(5)
    val totalRaised = raffleBox.getValue
    val winnerPercent = 100 - charityPercent - serviceFee
    val tokenId = raffleBox.getTokens.get(1).getId.toString
    val charityAddress = utils.getAddress(raffleBox.getRegisters.get(1).getValue.asInstanceOf[Coll[Byte]].toArray).toString
    // TODO : edit name and description
    new Raffle("", "", goal, totalRaised, deadlineHeight, serviceFee.toInt, charityPercent.toInt,
      charityAddress, winnerPercent.toInt, ticketPrice, totalSoldTicket, "", tokenId)
  }
}

case class Ticket(txId: String, tokenId: String, tokenCount: Long, walletAddress: String)

object Ticket{
  def apply(ticketBox: Json): Ticket = {
    val txId: String = ticketBox.hcursor.downField("transactionId").as[String].getOrElse("")
    val token = ticketBox.hcursor.downField("assets").as[Seq[Json]].getOrElse(null).head
    val tokenId: String = token.hcursor.downField("tokenId").as[String].getOrElse("")
    val tokenCount: Long = token.hcursor.downField("amount").as[Long].getOrElse(0)
    val registers = ticketBox.hcursor.downField("additionalRegisters").as[Json].getOrElse(null)
    val addressByte: Array[Byte] = ErgoValue.fromHex(registers.hcursor.downField("R4").as[Json].getOrElse(null)
      .hcursor.downField("serializedValue").as[String].getOrElse(""))
      .getValue.asInstanceOf[Coll[Byte]].toArray
    val walletAddress: String = Configs.addressEncoder.fromProposition(ErgoTreeSerializer.DefaultSerializer
      .deserializeErgoTree(addressByte)).get.toString
    new Ticket(txId, tokenId, tokenCount, walletAddress)
  }
}
