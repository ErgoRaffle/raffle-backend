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
import play.api.Logger
import special.collection.{Coll, CollOverArray}

import scala.collection.JavaConverters._
import scala.collection.mutable.Seq
import scala.util.control.Breaks.{break, breakable}

class CreateReqUtils @Inject()(client: Client, explorer: Explorer, utils: Utils, raffleContract: RaffleContract, addresses: Addresses,
                               createReqDAO: CreateReqDAO) {
  private val logger: Logger = Logger(this.getClass)

  def CreateRaffleProxyAddress(pk: String, charityPercent: Int, name: String, description: String, deadlineHeight: Long,
                               charityAddr: String, goal: Long, ticketPrice: Long): String = {
    client.getClient.execute(ctx => {
      val paymentAddress = addresses.getRaffleCreateProxyContract(pk, charityPercent, name, description, deadlineHeight, charityAddr, goal, ticketPrice)
      createReqDAO.insert(name, description, goal, deadlineHeight, charityPercent,
        charityAddr, ticketPrice, 0, pk, paymentAddress,
        null, null, 0, false,
        Configs.inf + utils.currentTime, Configs.creationDelay + utils.currentTime)

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
      val paymentBoxList = ctx.getCoveringBoxesFor(Address.create(req.paymentAddress), Configs.fee*4)
      if(!paymentBoxList.isCovered){
        throw new Throwable("Payment not covered the fee")
      }
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

      var change = paymentBoxList.getCoveredAmount - Configs.fee*4
      var fee = Configs.fee
      if(change <= Configs.minBoxErg) fee += change

      val inputBoxList = Seq(serviceBox) ++ paymentBoxList.getBoxes.asScala
      val raffleCreateTx = txB.boxesToSpend(inputBoxList.asJava)
        .fee(fee)
        .outputs(outputServiceBox, outputRaffleBox, outputTokenIssueBox)
        .sendChangeTo(Address.create(req.walletAddress).getErgoAddress)
        .build()

      val signedCreateTx = prover.sign(raffleCreateTx)
      logger.debug("raffle created and sent to network waiting to merge tokens to it")
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


  def isReady(req: CreateReq): Boolean = {
    client.getClient.execute(ctx => {
      val coveringList = ctx.getCoveringBoxesFor(Address.create(req.paymentAddress), 4*Configs.fee)
      if(coveringList.isCovered) {
        createReqDAO.updateTTL(req.id, utils.currentTime + Configs.creationDelay)
      }
      if (req.state == 0) {
        if(coveringList.isCovered) return true
      }
      else if (req.state == 1) {
        return utils.checkTransaction(req.createTxId.getOrElse("")) == 0
      }
      return false
    })
  }

  def generateAndSendBothTx(ctx: BlockchainContext, req: CreateReq): Unit = {
    try {
      val createTx = createRaffle(req)
      val mergeTx = mergeRaffle(createTx.getOutputsToSpend.get(1), createTx.getOutputsToSpend.get(2))
      ctx.sendTransaction(createTx)
      ctx.sendTransaction(mergeTx)
      createReqDAO.updateCreateTxID(req.id, createTx.getId)
      createReqDAO.updateMergeTxId(req.id, mergeTx.getId)
      createReqDAO.updateStateById(req.id, 1)
    } catch {
      case e:Throwable => logger.error(e.toString)
    }
  }

  def nextStage(req: CreateReq): Int = {
    client.getClient.execute(ctx => {
      if (req.state == 0) {
        generateAndSendBothTx(ctx, req)
        return 0
      } else if (req.state == 1) {
        val tx1Status = utils.checkTransaction(req.createTxId.getOrElse(""))
        if (tx1Status == 0) {
          generateAndSendBothTx(ctx, req)
        } else if (tx1Status == 1) {
            createReqDAO.updateStateById(req.id, 2)
        }
        return 0
      }
      else return 1
    })
  }

  def independentMergeTxGeneration(): Unit ={
    client.getClient.execute(ctx => {
      val raffleWaitingTokenAdd = Address.fromErgoTree(addresses.getRaffleWaitingTokenContract().getErgoTree, Configs.networkType)
      val raffleWaitingTokenBoxes = ctx.getCoveringBoxesFor(raffleWaitingTokenAdd, Configs.infBoxVal).getBoxes.asScala
        .filter(_.getTokens.size()>0)
        .filter(_.getTokens.get(0).getId.toString == Configs.token.service)
      raffleWaitingTokenBoxes.foreach(box => {
        val tokenId = new ErgoId(box.getRegisters.get(4).getValue.asInstanceOf[Coll[Byte]].toArray)
        val tokenBoxId = explorer.getUnspentTokenBoxes(tokenId.toString, 0, 100)
          .hcursor.downField("items").as[Seq[Json]].getOrElse(throw new Throwable("parse error")).head
          .hcursor.downField("boxId").as[String].getOrElse(null)
        val tokenBox = ctx.getBoxesById(tokenBoxId).head
        val mergeTx = mergeRaffle(box, tokenBox)
        val txId = ctx.sendTransaction(mergeTx)
        logger.info("Merge Tx sent with txId: "+ txId)
      })
    })
  }
}
