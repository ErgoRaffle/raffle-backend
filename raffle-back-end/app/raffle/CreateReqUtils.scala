package raffle

import dao.CreateReqDAO
import helpers.{Configs, Utils, connectionException, failedTxException, paymentNotCoveredException, proveException}
import io.circe.{Json, parser}
import network.{Client, Explorer}
import javax.inject.Inject
import models.CreateReq
import org.ergoplatform.appkit.impl.{ErgoTreeContract, InputBoxImpl}
import org.ergoplatform.appkit.{Address, BlockchainContext, ConstantsBuilder, ErgoId, ErgoToken, ErgoType, ErgoValue, InputBox, JavaHelpers, SignedTransaction}
import play.api.Logger
import special.collection.{Coll, CollOverArray}

import scala.collection.JavaConverters._
import scala.collection.mutable.Seq

class CreateReqUtils @Inject()(client: Client, explorer: Explorer, utils: Utils, raffleContract: RaffleContract, addresses: Addresses,
                               createReqDAO: CreateReqDAO) {
  private val logger: Logger = Logger(this.getClass)

  def CreateRaffleProxyAddress(pk: String, charityPercent: Int, name: String, description: String, deadlineHeight: Long,
                               charityAddr: String, goal: Long, ticketPrice: Long): String = {
    try {
      client.getClient.execute(ctx => {
        val paymentAddress = addresses.getRaffleCreateProxyContract(pk, charityPercent, name, description, deadlineHeight, charityAddr, goal, ticketPrice)
        createReqDAO.insert(name, description, goal, deadlineHeight, charityPercent, charityAddr, ticketPrice, 0, pk, paymentAddress,
          null, null, Configs.inf + utils.currentTime, Configs.creationDelay + utils.currentTime)
        paymentAddress
      })
    }
    catch {
      case e: Throwable => {
        logger.error(utils.getStackTraceStr(e))
        throw new Throwable("Error in payment address generation")
      }
    }
  }

  def createRaffle(req: CreateReq): SignedTransaction = {
    client.getClient.execute(ctx => {
      val txB = ctx.newTxBuilder()
      val prover = ctx.newProverBuilder()
        .build()
      val serviceBox = utils.getServiceBox()
      val paymentBoxList = client.getCoveringBoxesFor(Address.create(req.paymentAddress), Configs.fee*4)
      if(!paymentBoxList.isCovered) throw paymentNotCoveredException(s"Creation payment for request ${req.id} not covered the fee, request state id ${req.state} and request tx is ${req.createTxId}")

      val outputServiceBox = txB.outBoxBuilder()
        .value(serviceBox.getValue)
        .contract(new ErgoTreeContract(serviceBox.getErgoTree))
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

      val tokenName = ErgoValue.of(s"Raffle_token: ${req.name}".getBytes("utf-8"))
      val outputTokenIssueBox = txB.outBoxBuilder()
        .value(Configs.fee)
        .contract(addresses.getRaffleTokenIssueContract())
        .tokens(new ErgoToken(serviceBox.getId.getBytes, 1000000000L))
        .registers(tokenName, tokenName)
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

      try {
        val signedTx = prover.sign(raffleCreateTx)
        logger.info(s"create tx for request ${req.id} proved successfully")
        signedTx
      } catch {
        case e: Throwable => {
          logger.error(utils.getStackTraceStr(e))
          logger.error(s"create tx for request ${req.id} proving failed")
          throw proveException()
        }
      }
    })
  }

  def mergeRaffle(raffleBox: InputBox, tokenBox: InputBox): SignedTransaction = {
    client.getClient.execute((ctx: BlockchainContext) => {
      val txB = ctx.newTxBuilder()
      val prover = ctx.newProverBuilder()
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
          raffleBox.getTokens.get(0),
          tokenBox.getTokens.get(0)
        ).build()
      val tx = txB.boxesToSpend(Seq(raffleBox, tokenBox).asJava)
        .fee(Configs.fee)
        .outputs(raffleOutputBox)
        .sendChangeTo(Configs.serviceAddress.getErgoAddress)
        .build()
      try {
        val signedTx = prover.sign(tx)
        logger.info(s"merge tx for raffle ${tokenBox.getTokens.get(0).getId} proved successfully")
        signedTx
      } catch {
        case e: Throwable => {
          logger.error(utils.getStackTraceStr(e))
          logger.error(s"merge tx for raffle ${tokenBox.getTokens.get(0).getId} proving failed")
          throw proveException()
        }
      }
    })
  }


  def isReady(req: CreateReq): Boolean = {
    val coveringList = client.getCoveringBoxesFor(Address.create(req.paymentAddress), 4*Configs.fee)
    if(coveringList.isCovered) {
      createReqDAO.updateTTL(req.id, utils.currentTime + Configs.creationDelay)
    }
    if (req.state == 0) {
      if(coveringList.isCovered) return true
    }
    else if (req.state == 1) {
      return utils.checkTransaction(req.createTxId.getOrElse("")) == 0
    }
    false
  }

  def generateAndSendBothTx(ctx: BlockchainContext, req: CreateReq): Unit = {
    try {
      val createTx = createRaffle(req)
      var createTxId = ctx.sendTransaction(createTx)
      if (createTxId == null) throw failedTxException(s"Creation transaction sending failed for ${req.id}")
      else createTxId = createTxId.replaceAll("\"", "")
      logger.info(s"Creation transaction ${createTxId} sent for ${req.id}")
      createReqDAO.updateCreateTxID(req.id, createTxId)
      createReqDAO.updateStateById(req.id, 1)
      val mergeTx = mergeRaffle(createTx.getOutputsToSpend.get(1), createTx.getOutputsToSpend.get(2))
      var mergeTxId = ctx.sendTransaction(mergeTx)
      if (mergeTxId == null) throw failedTxException(s"Merge transaction sending failed for ${req.id}")
      else mergeTxId = mergeTxId.replaceAll("\"", "")
      logger.info(s"Merge transaction ${mergeTxId} sent for ${req.id}")
      createReqDAO.updateMergeTxId(req.id, mergeTxId)
    } catch {
      case e: connectionException => throw e
      case e: proveException => throw e
      case e:failedTxException => {
        logger.warn(e.getMessage)
        throw e
      }
      case e:paymentNotCoveredException => {
        logger.warn(e.getMessage)
        throw e
      }
      case e:Throwable => {
        logger.error(utils.getStackTraceStr(e))
        throw new Throwable("Error in new raffle creation")
      }
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
      val raffleWaitingTokenBoxes = client.getCoveringBoxesFor(raffleWaitingTokenAdd, Configs.infBoxVal).getBoxes.asScala
        .filter(_.getTokens.size() > 0)
        .filter(_.getTokens.get(0).getId.toString == Configs.token.service)
      raffleWaitingTokenBoxes.foreach(box => {
        try {
          val tokenId = new ErgoId(box.getRegisters.get(4).getValue.asInstanceOf[Coll[Byte]].toArray)
          val tokenBoxId = explorer.getUnspentTokenBoxes(tokenId.toString, 0, 100)
            .hcursor.downField("items").as[Seq[Json]].getOrElse(throw new Throwable("parse error")).head
            .hcursor.downField("boxId").as[String].getOrElse(null)
          val tokenBox = ctx.getBoxesById(tokenBoxId).head
          val mergeTx = mergeRaffle(box, tokenBox)
          if (utils.checkTransaction(mergeTx.getId) != 2) {
            val txId = ctx.sendTransaction(mergeTx)
            logger.info(s"Merge Tx for raffle ${tokenId} sent with txId: " + txId)
          }
        } catch {
          case e: Throwable => logger.warn(e.getMessage + s" in raffle mergeTx")
        }
      })
    })
  }
}
