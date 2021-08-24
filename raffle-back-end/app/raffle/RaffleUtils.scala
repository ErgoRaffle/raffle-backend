package raffle

import java.nio.charset.StandardCharsets

import helpers.Configs
import io.circe.Json
import javax.inject.Inject
import network.{Client, Explorer}
import org.ergoplatform.appkit.ErgoValue
import sigmastate.serialization.ErgoTreeSerializer
import special.collection.Coll

import scala.collection.mutable.{ListBuffer, Seq}
import scala.util.Try

class RaffleUtils @Inject()(client: Client, explorer: Explorer, addresses: Addresses) {

  def raffles(offset: Int, limit: Int): Json ={
    var raffleCount = 0
    var raffles: ListBuffer[Json] = ListBuffer()
    var explorerOffset: Int = 0
    var boxes = explorer.getUnspentTokenBoxes(Configs.token.service, 0, 100)
    val total = boxes.hcursor.downField("total").as[Int].getOrElse(0)
    while(raffleCount < offset+limit && explorerOffset < total) {
      Try {
        val items = boxes.hcursor.downField("items").as[Seq[Json]].getOrElse(throw new Throwable("parse error"))
          .filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null).size > 1)
          .filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null).head
            .hcursor.downField("tokenId").as[String].getOrElse("") == Configs.token.service)

        for (i <- items.indices) {
          raffleCount += 1
          if (raffleCount-1 >= offset && raffleCount <= offset + limit) {
            val id = items(i).hcursor.downField("assets").as[Seq[Json]].getOrElse(null)(1)
              .hcursor.downField("tokenId").as[String].getOrElse("")
            val registers = items(i).hcursor.downField("additionalRegisters").as[Json].getOrElse(null)
            val R4: Array[Long] = ErgoValue.fromHex(registers.hcursor.downField("R4").as[Json].getOrElse(null)
              .hcursor.downField("serializedValue").as[String].getOrElse(""))
              .getValue.asInstanceOf[Coll[Long]].toArray
            val charityPercent = R4(0)
            val serviceFee = R4(1)
            val winnerPercent = 100 - charityPercent - serviceFee
            val goal = R4(3)
            val deadlineHeight = R4(4)

            val strListByte: Array[Coll[Byte]] = ErgoValue.fromHex(registers.hcursor.downField("R6").as[Json].getOrElse(null)
              .hcursor.downField("serializedValue").as[String].getOrElse(""))
              .getValue.asInstanceOf[Coll[Coll[Byte]]].toArray
            val name: String = new String(strListByte(0).toArray, StandardCharsets.UTF_8)
            val description: String = new String(strListByte(1).toArray, StandardCharsets.UTF_8)
            raffles += Json.fromFields(List(
              ("id", Json.fromString(id)),
              ("name", Json.fromString(name)),
              ("description", Json.fromString(description)),
              ("deadline", Json.fromLong(deadlineHeight)),
              ("winnerPercent", Json.fromLong(winnerPercent)),
              ("charityPercent", Json.fromLong(charityPercent)),
              ("minToRaise", Json.fromLong(goal)),
            ))
          }
        }
      }
      explorerOffset += 100
      boxes = explorer.getUnspentTokenBoxes(Configs.token.service, explorerOffset, 100)
    }
    val currentHeight: Long = client.getHeight
    var totalRaffles = raffleCount - offset
    if (raffleCount > limit + offset) totalRaffles = limit
    else if (totalRaffles < 0) totalRaffles = 0

    Json.fromFields(List(
      ("items", Json.fromValues(raffles.toList)),
      ("total", Json.fromInt(totalRaffles)),
      ("currentHeight", Json.fromLong(currentHeight))
    ))
  }

  def userTickets(raffleId: String, wallerAdd: String): Json ={
    var C = 0
    var total = 100
    var selectedTickets: ListBuffer[Json] = ListBuffer()
    var totalTickets: Long = 0
    var totalRecords: Long = 0
    do {
      val response = explorer.getUnspentTokenBoxes(raffleId, C, 100)
      val tickets: Seq[Json] = response.hcursor.downField("items").as[Seq[Json]].getOrElse(null)
        .filter(_.hcursor.downField("assets").as[List[Json]].getOrElse(null).size == 1)
      total = response.hcursor.downField("total").as[Int].getOrElse(0)
      for (ticket <- tickets) {
        val registers = ticket.hcursor.downField("additionalRegisters").as[Json].getOrElse(null)
        val participantAddressByte: Array[Byte] = ErgoValue.fromHex(registers.hcursor.downField("R4").as[Json].getOrElse(null)
          .hcursor.downField("serializedValue").as[String].getOrElse(""))
          .getValue.asInstanceOf[Coll[Byte]].toArray
        val participantAddress = Configs.addressEncoder.fromProposition(ErgoTreeSerializer.DefaultSerializer
          .deserializeErgoTree(participantAddressByte)).get.toString
        if(participantAddress == wallerAdd){
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
    }while(C < total)
    Json.fromFields(List(
      ("items", Json.fromValues(selectedTickets.toList)),
      ("totalTickets", Json.fromLong(totalTickets)),
      ("total", Json.fromLong(totalRecords))
    ))
  }

  def raffleByTokenId(tokenId: String): Json={
    val raffleAdd = Configs.addressEncoder.fromProposition(addresses.getRaffleActiveContract().getErgoTree).get
    var boxes = explorer.getUnspentTokenBoxes(Configs.token.service, 0, 100)
    var items = boxes.hcursor.downField("items").as[Seq[Json]].getOrElse(throw new Throwable("parse error"))
      .filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null).size > 1)
      .filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null).head
        .hcursor.downField("tokenId").as[String].getOrElse("") == Configs.token.service)
      .filter(_.hcursor.downField("address").as[String].getOrElse("") == raffleAdd.toString)

    var c: Int = 1
    var item : Json = items.filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null)(1)
      .hcursor.downField("tokenId").as[String].getOrElse("") == tokenId).head

    while (item == null) {
      boxes = explorer.getUnspentTokenBoxes(Configs.token.service, c*100, 100)
      items = boxes.hcursor.downField("items").as[Seq[Json]].getOrElse(throw new Throwable("parse error"))
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

    val strListByte : Array[Coll[Byte]] = ErgoValue.fromHex(registers.hcursor.downField("R6").as[Json].getOrElse(null)
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
  }

}
