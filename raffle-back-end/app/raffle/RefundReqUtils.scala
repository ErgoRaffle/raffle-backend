package raffle

import dao.RefundReqDAO
import helpers.{Configs, Utils}
import javax.inject.Inject
import models.RefundReq
import network.{Client, Explorer}
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.appkit.{Address, ErgoToken, ErgoValue, InputBox}

import scala.collection.JavaConverters._


class RefundReqUtils  @Inject()(client: Client, refundReqDAO: RefundReqDAO){

  def Refund(req: RefundReq): Unit ={
    client.getClient.execute(ctx => {

      // TODO: Change this to chain the refund transactions
      val boxes = ctx.getUnspentBoxesFor(Address.create(req.raffleAddress))
      val raffleBox: InputBox = boxes.asScala.filter(box => box.getTokens.get(1).getId.toString == Configs.serviceTokenId)
        .filter(box => box.getTokens.get(0).getId.toString == req.raffleToken).head

      val ticketBox = ctx.getBoxesById(req.ticketBoxId).head
      if(ticketBox == null){
        refundReqDAO.updateStateById(req.id, 2)
        return
      }
//      val ticketBoxList = ctx.getUnspentBoxesFor(Address.create(req.ticketAddress))
//      val ticketBox: InputBox = ticketBoxList.asScala.filter(box => box.getTokens.get(0).getId.toString == req.raffleToken)
//        .filter(box => box.getTokens.get(0).getValue == req.ticketCount).head

      val participantAddress = ticketBox.getRegisters.get(2).toString

      val txB = ctx.newTxBuilder()

      // TODO : Change registers
      val newRaffleBox = txB.outBoxBuilder()
        .value(raffleBox.getValue - (req.ticketCount * req.ticketPrice))
        .contract(new ErgoTreeContract(Address.create(req.raffleAddress).getErgoAddress.script))
        .tokens(new ErgoToken(req.raffleToken, raffleBox.getTokens.get(0).getValue + req.ticketCount)
          , new ErgoToken(Configs.serviceTokenId, 1))
        .registers(ErgoValue.of(0L), ErgoValue.of(50L), ErgoValue.of(10L))
        .build()

      val refund = txB.outBoxBuilder()
        .value(req.ticketCount * req.ticketPrice)
        .contract(new ErgoTreeContract(Address.create(participantAddress).getErgoAddress.script))
        .build()

      val tx = txB.boxesToSpend(Seq(raffleBox, ticketBox).asJava)
        .fee(1000000)
        .outputs(newRaffleBox, refund)
        .sendChangeTo(Configs.serviceAddress.getErgoAddress)
        .build()

      val prover = ctx.newProverBuilder()
        .build()

      val signedTx = prover.sign(tx)
      refundReqDAO.updateStateById(req.id, 1)
      val txId = ctx.sendTransaction(signedTx)
      println("Refund Transaction Sent with TxId: ", txId)
    })
  }

  def isReady(req: RefundReq): Boolean ={
    client.getClient.execute(ctx => {
      if(req.state == 0) true
      else {
        val ticketBox = ctx.getBoxesById(req.ticketBoxId).head
        if(ticketBox == null) refundReqDAO.updateStateById(req.id, 2)
        return false
      }
    })
  }
}
