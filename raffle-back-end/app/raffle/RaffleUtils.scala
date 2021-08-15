package raffle

import helpers.{Configs, Utils}
import io.circe.Json
import network.{Client, Explorer}
import special.collection.Coll
import org.ergoplatform.appkit._
import org.ergoplatform.appkit.impl.ErgoTreeContract
import java.nio.charset.StandardCharsets
import java.util.Calendar

import dao.{ActiveRafflesDAO, RefundReqDAO}
import javax.inject.Inject
import models.ActiveRaffle
import play.api.Logger
import sigmastate.serialization.ErgoTreeSerializer

import scala.collection.JavaConverters._
import scala.collection.mutable.Seq
import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.util.control.Breaks.{break, breakable}


class RaffleUtils @Inject()(client: Client, explorer: Explorer, utils: Utils, raffleContract: RaffleContract,
                            activeRafflesDAO: ActiveRafflesDAO, refundReqDAO: RefundReqDAO) {
  private val logger: Logger = Logger(this.getClass)

  def findRecentBox(address: String, tokenId: String): InputBox ={
    val response : Json = explorer.getUnconfirmedTxByAddress(address)
    val items = response.hcursor.downField("items").as[List[Json]].getOrElse(throw new Throwable("bad request"))
    client.getClient.execute(ctx => {
      if(items.nonEmpty) {
        var txJson: Json = null
        var timestamp: Long = 100000000000000000L
        var index: Int = 0
        for (item <- items) {
          item.hcursor.downField("outputs").as[List[Json]].getOrElse(null).map(output => {
            try {
              val asset = output.hcursor.downField("assets").as[List[Json]].getOrElse(throw new Throwable("somethings wrong"))
                .headOption.getOrElse(throw new Throwable("list is empty"))
              if (asset.hcursor.downField("tokenId").as[String].getOrElse("") == tokenId) {
                val timestampNew: Long = item.hcursor.downField("creationTimestamp").as[Long].getOrElse(0)
                if (timestampNew < timestamp) {
                  timestamp = timestampNew
                  txJson = item
                  index = output.hcursor.downField("index").as[Int].getOrElse(0)
                }
              }
            } catch {
              case e: Throwable => println(e)
            }
          })
        }
        if (txJson != null) {
          var newJson = txJson.toString().replaceAll("id", "boxId")
            .replaceAll("txId", "transactionId")
            .replaceAll("null", "\"\"")
          newJson = newJson.substring(0, 5) + "id" + newJson.substring(10)

          val signedTx = ctx.signedTxFromJson(newJson)
          val sTx = signedTx.getOutputsToSpend.get(index)
          return sTx
        }
      }

      // If the box not found in mempool find it from UTXO set
      val listBoxes = ctx.getUnspentBoxesFor(Address.create(address))
      // TODO: Change it random selection if multiple service boxes created
      listBoxes.asScala.filter(box => box.getTokens.size() > 0)
        .filter(box => box.getTokens.get(0).getId.toString == tokenId).head
    })
  }

  def createService(): String = {
    client.getClient.execute(ctx => {
      val prover = ctx.newProverBuilder()
        .withDLogSecret(Configs.serviceSecret)
        .build()

      val txB = ctx.newTxBuilder()
      val serviceContract = ctx.compileContract(
        ConstantsBuilder.create()
          .item("servicePubKey", Configs.serviceAddress.getPublicKey)
          .build(),
        raffleContract.raffleServiceScript)

      val listBoxes = ctx.getUnspentBoxesFor(Configs.serviceAddress)
      val serviceBox: InputBox = listBoxes.asScala.filter(box => box.getValue >= 9000000000L).head
      println("serviceBox value: " + serviceBox.getValue)

      val out = txB.outBoxBuilder()
        .contract(serviceContract)
        .tokens(new ErgoToken(serviceBox.getId, 1000000000000000000L))
        .value(serviceBox.getValue - Configs.fee)
        .build()

      val tx = txB.boxesToSpend(Seq(serviceBox).asJava)
        .fee(Configs.fee)
        .outputs(out)
        .sendChangeTo(Configs.serviceAddress.getErgoAddress)
        .build()

      val signedTx = prover.sign(tx)

      val txId = ctx.sendTransaction(signedTx)
      println("tokenId: " + serviceBox.getId)
      return txId
    })
  }

  def isReady(raffle: ActiveRaffle): Boolean ={
    client.getClient.execute(ctx => {
      if(raffle.state == 0) {
        // Check if the raffle is finished or not
        val currentHeight = ctx.getHeight
        if (raffle.deadlineHeight >= currentHeight) return true
        else return false
      }
      else if(raffle.state == 1) return true
      else if(raffle.state == 2) {
        // Check if raffle successfully destroyed
        val boxes = ctx.getUnspentBoxesFor(Address.create(raffle.raffleAddress))
        val raffleBox: InputBox = boxes.asScala.filter(box => box.getTokens.get(1).getId.toString == Configs.serviceTokenId)
          .filter(box => box.getTokens.get(0).getId.toString == raffle.raffleToken).head

        if (raffleBox == null) {
          // Find the winner box for next round (winner funding)
          var c: Int = 0
          val winnerContract = ctx.compileContract(
            ConstantsBuilder.create()
              .build(),
            raffleContract.winnerScript)
          val winnerBoxList = ctx.getUnspentBoxesFor(Address.fromErgoTree(winnerContract.getErgoTree, Configs.networkType))
          val winnerBox = winnerBoxList.asScala.filter(box => box.getTokens.get(0).getId.toString == raffle.raffleToken).head
          activeRafflesDAO.updateWinnerBoxId(raffle.id, winnerBox.getId.toString)
          activeRafflesDAO.updateStateById(raffle.id, 3)
          return true
        }
      }
      else if(raffle.state == 3) {
        // Check if winner successfully funded
        val winnerBox = ctx.getBoxesById(raffle.winnerBoxId)
        if(winnerBox == null) activeRafflesDAO.updateStateById(raffle.id, 10)
      }
      else if(raffle.state == 8) {
        // Check if the raffle refund phase is finished or not
        val boxes = ctx.getUnspentBoxesFor(Address.create(raffle.raffleAddress))
        val raffleBox: InputBox = boxes.asScala.filter(box => box.getTokens.get(1).getId.toString == Configs.serviceTokenId)
          .filter(box => box.getTokens.get(0).getId.toString == raffle.raffleToken).head

        // TODO: Change it with register check
        if(raffleBox.getValue == Configs.fee) {
          activeRafflesDAO.updateStateById(raffle.id, 9)
          return true
        }
      }
      else if(raffle.state == 9) {
        // Check if raffle successfully destroyed
        val boxes = ctx.getUnspentBoxesFor(Address.create(raffle.raffleAddress))
        val raffleBox: InputBox = boxes.asScala.filter(box => box.getTokens.get(1).getId.toString == Configs.serviceTokenId)
          .filter(box => box.getTokens.get(0).getId.toString == raffle.raffleToken).head

        if(raffleBox == null) activeRafflesDAO.updateStateById(raffle.id, 10)
      }
      false
    })
  }

  def raffleFinishProcess(raffle: ActiveRaffle): Unit = {
    client.getClient.execute(ctx => {
      val boxes = ctx.getUnspentBoxesFor(Address.create(raffle.raffleAddress))
      val raffleBox: InputBox = boxes.asScala.filter(box => box.getTokens.get(1).getId.toString == Configs.serviceTokenId)
        .filter(box => box.getTokens.get(0).getId.toString == raffle.raffleToken).head

      if(raffleBox.getValue >= raffle.minToRaise * 1000000){
        activeRafflesDAO.updateStateById(raffle.id, 1)
      }
      else {
        var response = explorer.getUnspentTokenBoxes(Configs.serviceTokenId, 1, 100)
        var items = response.hcursor.downField("items").as[List[Json]].getOrElse(null)

        var c: Int = 1
        while (items != null && items.nonEmpty) {
          items.foreach(box => {
            val address = box.hcursor.downField("address").as[String].getOrElse("")
            val asset = box.hcursor.downField("assets").as[Seq[Json]].getOrElse(null).head
            val id = asset.hcursor.downField("tokenId").as[String].getOrElse("")
            if (id == raffle.raffleToken && address != raffle.raffleAddress) {
              val ticketCount: Long = asset.hcursor.downField("amount").as[Long].getOrElse(0)
              val boxId = box.hcursor.downField("boxId").as[String].getOrElse("")
              refundReqDAO.insert(ticketCount, raffle.ticketPrice, 0, raffle.raffleAddress, raffle.raffleToken,
                null, boxId, 0)
            }
          })
          response = explorer.getUnspentTokenBoxes(Configs.serviceTokenId, c*100, 100)
          items = response.hcursor.downField("items").as[List[Json]].getOrElse(null)
          c += 1
        }
        activeRafflesDAO.updateStateById(raffle.id, 8)
      }
    })
  }

  def winnerAnnouncement(raffle: ActiveRaffle): Unit ={
    client.getClient.execute(ctx => {
      val boxes = ctx.getUnspentBoxesFor(Address.create(raffle.raffleAddress))
      val raffleBox: InputBox = boxes.asScala.filter(box => box.getTokens.get(1).getId.toString == Configs.serviceTokenId)
        .filter(box => box.getTokens.get(0).getId.toString == raffle.raffleToken).head

      // TODO Change here to use mempool
      val boxes2 = ctx.getUnspentBoxesFor(Configs.serviceAddress)
      val serviceBox: InputBox = boxes2.asScala.filter(box => box.getTokens.size() > 1)
        .filter(box => box.getTokens.get(0).getId.toString == Configs.serviceTokenId)
        .filter(box => box.getTokens.get(0).getValue > 1000).head

      val txB = ctx.newTxBuilder()
      // TODO Change here with registers
      val raffleTotalRaised = raffle.ticketPrice // * register count

      val newServiceBox = txB.outBoxBuilder()
        .value(serviceBox.getValue + raffleTotalRaised * Configs.servicePercent / 100)
        .contract(new ErgoTreeContract(Configs.serviceAddress.getErgoAddress.script))
        .tokens(new ErgoToken(Configs.serviceTokenId, serviceBox.getTokens.get(0).getValue + 1))
        .build()

      val charityPercent = 100 - Configs.servicePercent - raffle.winnerPercent
      val charityBox = txB.outBoxBuilder()
        .value(raffleTotalRaised * charityPercent / 100)
        .contract(new ErgoTreeContract(Address.create(raffle.charityAdd).getErgoAddress.script))
        .build()

      // TODO: from Registers
      val soldTokens: Int = 600
      val oracleBoxList = explorer.getUnspentTokenBoxes(Configs.oracleId,0,100)
      val oracleBoxId = oracleBoxList.hcursor.downField("items").as[Seq[Json]].getOrElse(null).head
        .hcursor.downField("boxId").as[String].getOrElse("")
      val winner = ((soldTokens + BigInt(oracleBoxId.slice(0, 15))) % soldTokens).toLong
      val winnerContract = ctx.compileContract(
        ConstantsBuilder.create()
          .build(),
        raffleContract.winnerScript)
      val winnerBox = txB.outBoxBuilder()
        .value(raffleTotalRaised * raffle.winnerPercent / 100)
        .tokens(new ErgoToken(raffle.raffleToken, raffleBox.getTokens.get(0).getValue))
        .registers(ErgoValue.of(winner))
        .contract(winnerContract)
        .build()

      val tx = txB.boxesToSpend(Seq(serviceBox, raffleBox).asJava)
        .fee(Configs.fee)
        .outputs(newServiceBox, charityBox, winnerBox)
        .sendChangeTo(Configs.serviceAddress.getErgoAddress)
        .build()

      val prover = ctx.newProverBuilder()
        .build()

      val signedTx = prover.sign(tx)
      val txId = ctx.sendTransaction(signedTx)
      println("Final Transaction Sent with TxId: ", txId)
      activeRafflesDAO.updateStateById(raffle.id, 2)
      // TODO: Winner add to database for funding
    })
  }

  def destroyAfterRefund(raffle: ActiveRaffle): Unit ={
    client.getClient.execute(ctx => {
      val boxes = ctx.getUnspentBoxesFor(Address.create(raffle.raffleAddress))
      val raffleBox: InputBox = boxes.asScala.filter(box => box.getTokens.get(1).getId.toString == Configs.serviceTokenId)
        .filter(box => box.getTokens.get(0).getId.toString == raffle.raffleToken).head

      val serviceBox: InputBox = findRecentBox(Configs.serviceAddress.toString, Configs.serviceTokenId)

      val txB = ctx.newTxBuilder()

      val newServiceBox = txB.outBoxBuilder()
        .value(serviceBox.getValue + raffleBox.getValue - Configs.fee)
        .contract(new ErgoTreeContract(Configs.serviceAddress.getErgoAddress.script))
        .tokens(new ErgoToken(Configs.serviceTokenId, serviceBox.getTokens.get(0).getValue + 1))
        .build()

      val tx = txB.boxesToSpend(Seq(serviceBox, raffleBox).asJava)
        .fee(Configs.fee)
        .outputs(newServiceBox)
        .sendChangeTo(Configs.serviceAddress.getErgoAddress)
        .tokensToBurn(new ErgoToken(raffleBox.getTokens.get(0).getId, raffleBox.getTokens.get(0).getValue))
        .build()

      val prover = ctx.newProverBuilder()
        .build()

      val signedTx = prover.sign(tx)
      activeRafflesDAO.updateStateById(raffle.id, 9)
      val txId = ctx.sendTransaction(signedTx)
      println("Final Transaction Sent with TxId: ", txId)
    })
  }

  def winnerFund(raffle: ActiveRaffle): Unit ={
    client.getClient.execute(ctx => {
      val winnerBox = ctx.getBoxesById(raffle.winnerBoxId).head
      val winnerId : Long = winnerBox.getRegisters.get(0).asInstanceOf[Long]

      var response = explorer.getUnspentTokenBoxes(Configs.serviceTokenId, 1, 100)
      var items = response.hcursor.downField("items").as[List[Json]].getOrElse(null)

      var c: Int = 1
      breakable {
        while (items != null && items.nonEmpty) {
          items.foreach(box => {
            val registers = box.hcursor.downField("additionalRegisters").as[Json].getOrElse(null)
            if (!registers.isNull) {
              val R4 = registers.hcursor.downField("R4").as[Json].getOrElse(null)
              val R4Value: Long = R4.hcursor.downField("renderedValue").as[Long].getOrElse(0)
              val R5 = registers.hcursor.downField("R4").as[Json].getOrElse(null)
              val R5Value: Long = R5.hcursor.downField("renderedValue").as[Long].getOrElse(0)
              if (R4Value < winnerId && R5Value >= winnerId) {

                val boxId = box.hcursor.downField("boxId").as[String].getOrElse("")
                val ticketBox = ctx.getBoxesById(boxId).head

                val winnerAddress: String = ticketBox.getRegisters.get(2).asInstanceOf[Coll[Byte]].toString()
                val txB = ctx.newTxBuilder()

                val winnerFund = txB.outBoxBuilder()
                  .value(winnerBox.getValue)
                  .contract(new ErgoTreeContract(Address.create(winnerAddress).getErgoAddress.script))
                  .tokens()
                  .build()

                val tx = txB.boxesToSpend(Seq(winnerBox, ticketBox).asJava)
                  .fee(Configs.fee)
                  .outputs(winnerFund)
                  .tokensToBurn(new ErgoToken(winnerBox.getTokens.get(0).getId, winnerBox.getTokens.get(0).getValue))
                  .sendChangeTo(Configs.serviceAddress.getErgoAddress)
                  .build()

                val prover = ctx.newProverBuilder()
                  .build()

                val signedTx = prover.sign(tx)
                val txId = ctx.sendTransaction(signedTx)
                println("Final winner refund Transaction Sent with TxId: ", txId)
                break
              }
            }
          })
          response = explorer.getUnspentTokenBoxes(Configs.serviceTokenId, 1, 100)
          items = response.hcursor.downField("items").as[List[Json]].getOrElse(null)
          c += 1
        }
      }
    })
  }

  def nextStage(raffle: ActiveRaffle): Unit ={
    if(raffle.state == 0) raffleFinishProcess(raffle)
    else if(raffle.state == 1 || raffle.state == 2) winnerAnnouncement(raffle)
    else if(raffle.state == 3) winnerFund(raffle)
    else if(raffle.state == 9) destroyAfterRefund(raffle)
  }

  def raffleSearch(): Unit = {
    logger.debug("Searching for new raffles started")

    // TODO : raffle address must be replaced
    val raffleAdd = "2qheKziy2VUtAjCVNMuNgbPrqcEXXtd5FRKuYwU5Z4WtZBmAtFPLb8yq21a4AQBNRcSQwQ5ryz8Am4WWTDuJfAWnDmE6J5h1ruSDkizyAqSnyByNW8PzPy1Y7xCmq2PBg4cZzJqnQmbbZq6x7J5hNoQv6i96CXk3YMVJMYdzqNQq8hBSicBTEAbbpfgqQJMrtTaAAQSHWoxbBp5R75bAwvoje4qQZGKRTpJbChukxYLBQDsmrfNTDeSxqJUPFZHZ6ZBfT27ia7qToBnmAUaKCzJ5AqniBWvmmcmi2ramGZik9zQd1wtrJSpyb4CZy2JPHnXJp6fG3RwEQjJvZbgcgVb9VVYvF9imc1rrJ3BdKu7jrwXP84dBAjQuqqpKJts4Ay8ncMw2pBabh52r9gtUFhDRbyQYPacBUsbBqNosJAiaXvQqjUxUFA2qzfkW5E6UWp5XDLtPtxANYHC9o8oVduapauSoCK3gEvqJnptn11JaRwzwjcjB4agK1Xj5xmDvAeq9TcEXnwMAELfso5y6EDWgHJRnEjbMMfAMQRmS2cJYeCJQKnymsuivWJob1xuB1PcNkHCXJHn3zCetJiCnrcKyzZ3y7DTV8aX2Mmr5NnphQdxuPxawhqEGGSfspCPMU2qGDQp9A4gmvN96WAqk98gzihYwwkgEM4xAzcXrhxbfF3RYcu7b7uscto96hqMrVQ258UyRdqUpQUcrYWeYQpoD78Zwesytx9oWK8gJL6Q5GFrHD8Zx2T6LtLh12oeHbxhe2hjnAd2WBtS82k5jpnXZNSyrqE22uwoDzS5kruw11ZdB8gjTrohPmyHJvWCZTXfdw1N8J2nghD67NJJy3JJikvY1JpncQ8somidi6gdLKene8KCvoimLTPgWXK1oK3m34HjsfHayZbrNhmviWpgGDL7gY1FJrVe9BjP6LfMe8QVfd82yFsye8YE979H8mCuPjqR4enG4iN61SnhjevkMkyNhTQu6XkdjgLrsHhW341um3FEHs3Ay2C7njfK92vBDPiXoWBmeb5poGKd74Ue7zD3UtXCQuiesNmXR2KtDfeYY6R1nd2J36J8RX6DJqd39ajy29VNt7FALQvZ21bWFAAb4ExVQTGjX7DkqeLXQgK84yQPC9yeY4DFeLok4HujVry81YXEaat9wHqMga3BNigzLAD3p8QZobNdRSB6Cd5iY6hhYsvnhmcGdrEAYnLn1mZ4bW4RpVUHq5HyXdzHSJC7Ap9ZSvxqgvhHor6KAFBdymm1CXpXXQ2uYkgziB3BzYJBQk7Ngmoc2qQV9WrmAJRZCSNPd7kqNm7rveZn9DBM6LbjT7xFPxKUGAegkwHbeWNM1P5QSGPJUtW1ciGm9p84hR5jNcWbKJyJYjjrZ7Z"
    var response = explorer.getUnspentTokenBoxes(Configs.token.service, 0, 100)
    var items = response.hcursor.downField("items").as[List[Json]].getOrElse(null)
      .filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null).head
        .hcursor.downField("tokenId").as[String].getOrElse("") == Configs.token.service)
      .filter(_.hcursor.downField("address").as[String].getOrElse("") == raffleAdd)
    activeRafflesDAO.updatingStatus()

    var c: Int = 1
    while (items != null && items.nonEmpty) {
      items.foreach(box => {
        val asset = box.hcursor.downField("assets").as[Seq[Json]].getOrElse(null)(1)
        val id = asset.hcursor.downField("tokenId").as[String].getOrElse("")
        try {
          val raffle = activeRafflesDAO.byTokenId(id)
          activeRafflesDAO.acceptUpdating(raffle.id)
          // TODO : update raffle state from address
        }
        catch{
          case e: Throwable => {
            logger.debug("New raffle found with Token Id: " + id)
            val registers = box.hcursor.downField("additionalRegisters").as[Json].getOrElse(null)
            val R4: Array[Long] = ErgoValue.fromHex(registers.hcursor.downField("R4").as[Json].getOrElse(null)
              .hcursor.downField("serializedValue").as[String].getOrElse(""))
              .getValue.asInstanceOf[Coll[Long]].toArray
            val charityCoef = R4(0)
            val ticketPrice = R4(2)
            val goal = R4(3)
            val deadlineHeight = R4(4)

            val charityAddressByte: Array[Byte] = ErgoValue.fromHex(registers.hcursor.downField("R5").as[Json].getOrElse(null)
              .hcursor.downField("serializedValue").as[String].getOrElse(""))
              .getValue.asInstanceOf[Coll[Byte]].toArray
            val charityAddress = Configs.addressEncoder.fromProposition(ErgoTreeSerializer.DefaultSerializer
              .deserializeErgoTree(charityAddressByte)).get.toString

            logger.debug("New raffle data: " + charityCoef +", "+ charityAddress +", "+ ticketPrice +", "+ goal +", "+ deadlineHeight)
            // TODO : state by address
            val address = box.hcursor.downField("address").as[String].getOrElse("")
            activeRafflesDAO.insert(goal, deadlineHeight, (100 - charityCoef).toInt, charityAddress, ticketPrice, 0,
              address, id, "not-imp", 0)
          }
        }
      })
      response = explorer.getUnspentTokenBoxes(Configs.token.service, c*100, 100)
      items = response.hcursor.downField("items").as[List[Json]].getOrElse(null)
        .filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null).head
          .hcursor.downField("tokenId").as[String].getOrElse("") == Configs.token.service)
        .filter(_.hcursor.downField("address").as[String].getOrElse("") == raffleAdd)
      c += 1
    }

    activeRafflesDAO.deleteAfterUpdating()
    logger.debug("Searching for new raffles finished")
  }
}