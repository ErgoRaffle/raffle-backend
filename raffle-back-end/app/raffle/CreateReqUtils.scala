package raffle

import java.util.Calendar
import org.ergoplatform.appkit.impl.ScalaBridge
import org.ergoplatform.restapi.client.ErgoTransaction

import dao.CreateReqDAO
import helpers.{Configs, Utils}
import io.circe.{Json, parser}
import network.{Client, Explorer}

import javax.inject.Inject
import models.CreateReq
import org.ergoplatform.appkit.impl.{ErgoTreeContract, InputBoxImpl}
import org.ergoplatform.appkit.{Address, BlockchainContext, ConstantsBuilder, ErgoId, ErgoToken, ErgoType, ErgoValue, InputBox, JavaHelpers, SignedTransaction}
import scorex.crypto.hash.Digest32
import io.circe.syntax._
import special.collection.{Coll, CollOverArray}

import scala.collection.JavaConverters._
import scala.collection.mutable.Seq
import scala.util.control.Breaks.{break, breakable}

class CreateReqUtils @Inject()(client: Client, explorer: Explorer, utils: Utils, raffleContract: RaffleContract, addresses: Addresses,
                               createReqDAO: CreateReqDAO) {
  var serviceBox: InputBox = _

  def updateServiceBox() = {
    client.getClient.execute(ctx => {
      val listBoxes = ctx.getUnspentBoxesFor(Configs.serviceAddress, 0, 100)
      println(listBoxes.size)
      for (i <- 0 until listBoxes.size()) {
        if (listBoxes.get(i).getTokens.size() >= 1 && listBoxes.get(i).getTokens.get(0).getId.toString == Configs.serviceTokenId
          && listBoxes.get(i).getTokens.get(0).getValue >= 100000000L) {
          serviceBox = listBoxes.get(i)
        }
      }
    })
  }

  def CreateRaffleProxyAddress(pk: String, charityPercent: Int, name: String, description: String, deadlineHeight: Long,
                               charityAddr: String, goal: Long, ticketPrice: Long): String = {
    client.getClient.execute(ctx => {
      val paymentAddress = addresses.getRaffleCreateProxyContract(pk, charityPercent, name, description, deadlineHeight, charityAddr, goal, ticketPrice)
      val currentTime = Calendar.getInstance().getTimeInMillis / 1000
      createReqDAO.insert(name, description, goal, deadlineHeight, charityPercent,
        charityAddr, ticketPrice, 0, pk, paymentAddress,
        null, null, 0, false,
        Configs.inf + currentTime, Configs.creationDelay + currentTime)

      return paymentAddress
    })
  }

  def createRaffle(req: CreateReq): SignedTransaction = {
    client.getClient.execute(ctx => {
      val txB = ctx.newTxBuilder()
      val prover = ctx.newProverBuilder()
        .withDLogSecret(Configs.serviceSecret)
        .build()
      val serviceBox = utils.getServiceBox()
      val proxyBoxes = ctx.getUnspentBoxesFor(Address.create(req.paymentAddress), 0, 100).asScala.toSeq
      val paymentBoxList = Seq(serviceBox) ++ proxyBoxes
      val total = proxyBoxes.map(item => item.getValue).reduce((a, b) => a + b)
      val outputServiceBox = txB.outBoxBuilder()
        .value(serviceBox.getValue)
        .contract(addresses.getRaffleServiceContract())
        .tokens(
          new ErgoToken(Configs.token.nft, 1),
          new ErgoToken(Configs.token.service, serviceBox.getTokens.get(1).getValue - 1)
        )
        .registers(
          serviceBox.getRegisters.get(0),
          serviceBox.getRegisters.get(1),
        )
        .build()
      val serviceFee = serviceBox.getRegisters.get(0).getValue.asInstanceOf[Long]
      val r4 = utils.longListToErgoValue(Array(req.charityPercent, serviceFee, req.ticketPrice, req.goal, req.deadlineHeight, 0L))
      val r5 = ErgoValue.of(new ErgoTreeContract(Address.create(req.charityAddr).getErgoAddress.script).getErgoTree.bytes)
      val r6 = ErgoValue.of(Seq(req.name.getBytes("utf-8"), req.description.getBytes("utf-8"), serviceBox.getId.getBytes).map(item => {
        ErgoValue.of(IndexedSeq(item: _*).toArray)
      }).map(item => item.getValue).toArray, ErgoType.collType(ErgoType.byteType()))
      val r7 = ErgoValue.of(serviceBox.getRegisters.get(1).getValue.asInstanceOf[Coll[Byte]].toArray.clone())
      val r8 = ErgoValue.of(serviceBox.getId.getBytes.clone())
      val outputRaffleBox = txB.outBoxBuilder()
        .value(Configs.fee * 2)
        .contract(addresses.getRaffleWaitingTokenContract())
        .tokens(new ErgoToken(Configs.token.service, 1))
        .registers(r4, r5, r6, r7, r8).build()
      val outputTokenIssueBox = txB.outBoxBuilder()
        .value(Configs.fee)
        .contract(addresses.getRaffleTokenIssueContract())
        .tokens(new ErgoToken(serviceBox.getId.getBytes, 1000000000L))
        .build()

      var raffleCreateTxBuilder = txB.boxesToSpend(paymentBoxList.asJava)
        .fee(Configs.fee)

      if (total > Configs.fee * 4) {
        val changeOutput = txB.outBoxBuilder()
          .value(total - Configs.fee * 4)
          .contract(new ErgoTreeContract(Address.create(req.walletAddress).getErgoAddress.script))
          .build()
        raffleCreateTxBuilder = raffleCreateTxBuilder.outputs(outputServiceBox, outputRaffleBox, outputTokenIssueBox, changeOutput)
      } else {
        raffleCreateTxBuilder = raffleCreateTxBuilder.outputs(outputServiceBox, outputRaffleBox, outputTokenIssueBox)
      }
      val raffleCreateTx = raffleCreateTxBuilder
        .sendChangeTo(Address.create(req.walletAddress).getErgoAddress)
        .build()
      val signedCreateTx = prover.sign(raffleCreateTx)
      println("raffle created and sent to network waiting to merge tokens to it")
      signedCreateTx
    })
  }

  def mergeRaffle(raffleBox: InputBox, tokenBox: InputBox): SignedTransaction = {
    client.getClient.execute((ctx: BlockchainContext) => {
      val txB = ctx.newTxBuilder()
      val prover = ctx.newProverBuilder()
        .withDLogSecret(Configs.serviceSecret)
        .build()
      val raffleOutputBox = txB.outBoxBuilder()
        .value(Configs.fee * 2)
        .contract(addresses.getRaffleActiveContract())
        .registers(
          raffleBox.getRegisters.get(0),
          raffleBox.getRegisters.get(1),
          raffleBox.getRegisters.get(2),
          raffleBox.getRegisters.get(3),
        )
        .tokens(
          new ErgoToken(Configs.token.service, 1),
          tokenBox.getTokens.get(0)
        ).build()
      val tx = txB.boxesToSpend(Seq(raffleBox, tokenBox).asJava)
        .fee(Configs.fee)
        .outputs(raffleOutputBox)
        .sendChangeTo(Configs.serviceAddress.getErgoAddress)
        .build()
      prover.sign(tx)
    })
  }

  // TODO: ONLY REMOVE THE REQUEST IF PAYMENT BOX IS UNAVAILABLE
  def isReady(req: CreateReq): Boolean = {
    val currentTime = Calendar.getInstance().getTimeInMillis / 1000
    client.getClient.execute(ctx => {
      if (req.state == 0) {
        val paymentBoxList = ctx.getUnspentBoxesFor(Address.create(req.paymentAddress), 0, 100)
        if (paymentBoxList.size() == 0) return false
        val total = paymentBoxList.asScala.map(item => item.getValue).reduce((a, b) => a + b)
        return total >= 4 * Configs.fee
      } else if (req.state == 1) {
        return checkTransaction(req.createTxId.getOrElse("")) == 0 || checkTransaction(req.mergeTxId.getOrElse("")) == 0
      }
      return false
    })
  }

  def isValid(req: CreateReq): Boolean = {
    if (req.state == 0) return false
    else if (req.state == 1 || req.state == 2) {
      // TODO: Update the code to query the database and check chained service box
      //      if (req.newServiceBox == serviceBox.getId.toString) return true
      //      println("Service Box have Changed, Transaction should be created with new box.")
      //      println("new service box: ", relatedServiceBox, "old service box: ", serviceBox)
      return false
    }
    false
  }

  def update(req: CreateReq): Unit = {
    println("updating the request")
    if (req.state == 0) {
      //      val signedTxJson: Option[String] = raffleTokenIssue(req)
      //      addRaffle(req, signedTxJson)
    }
    else if (req.state == 1 || req.state == 2) {
      client.getClient.execute(ctx => {
        //        addRaffle(req, req.signedProxyTxJson)
      })
    }
  }

  def checkTransaction(txId: String): Int = {
    if(txId != "") {
      val unconfirmedTx = explorer.getUnconfirmedTx(txId)
      if (unconfirmedTx == Json.Null) {
        val confirmedTx = explorer.getConfirmedTx(txId)
        if (confirmedTx == Json.Null) {
          0 // resend transaction
        } else {
          1 // transaction mined
        }
      } else {
        2 // transaction already in mempool
      }
    }else{
      0
    }
  }

  def generateAndSendBothTx(ctx: BlockchainContext, req: CreateReq): Unit = {
    val createTx = createRaffle(req)
    val mergeTx = mergeRaffle(createTx.getOutputsToSpend.get(1), createTx.getOutputsToSpend.get(2))
    ctx.sendTransaction(createTx)
    ctx.sendTransaction(mergeTx)
    createReqDAO.updateCreateTxID(req.id, createTx.getId)
    createReqDAO.updateMergeTxId(req.id, mergeTx.getId)
    createReqDAO.updateStateById(req.id, 1)
  }

  def generateAndSendMergeTx(ctx: BlockchainContext, req: CreateReq): Unit = {
    val txJson = explorer.getConfirmedTx(req.createTxId.getOrElse(""))
    // TODO write better solution and parse transaction json
    val outputs = txJson.hcursor.downField("outputs").as[List[Json]].getOrElse(throw new Throwable("Invalid transaction found")).toArray
    val raffleBoxId = outputs(1).hcursor.downField("boxId").as[String].getOrElse("")
    val tokenBoxId = outputs(2).hcursor.downField("boxId").as[String].getOrElse("")
    val raffleBox = ctx.getBoxesById(raffleBoxId).head
    val tokenBox = ctx.getBoxesById(tokenBoxId).head
    val mergeTx = mergeRaffle(raffleBox, tokenBox)
    val txId2 = ctx.sendTransaction(mergeTx)
    createReqDAO.updateMergeTxId(req.id, txId2)
    createReqDAO.updateStateById(req.id, 1)
  }

  def nextStage(req: CreateReq): Int = {
    client.getClient.execute(ctx => {
      if (req.state == 0) {
        generateAndSendBothTx(ctx, req)
        return 0
      } else if (req.state == 1) {
        val tx1Status = checkTransaction(req.createTxId.getOrElse(""))
        if (tx1Status == 0) {
          generateAndSendBothTx(ctx, req)
        } else if (tx1Status == 1) {
          val tx2Status = checkTransaction(req.mergeTxId.getOrElse(""))
          if (tx2Status == 0) {
            generateAndSendMergeTx(ctx, req)
          } else if (tx2Status == 1) {
            // both transactions mined and raffle is currently active
            createReqDAO.updateStateById(req.id, 2)
          }
        }
        return 0
      }
      else return 1
    })
  }

}
