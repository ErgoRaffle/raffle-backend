package raffle

import dao.CreateReqDAO
import helpers.{Configs, Utils}
import network.{Client, Explorer}
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.appkit.{Address, BlockchainContext, ConstantsBuilder, ErgoContract, ErgoId}
import scorex.crypto.hash.Digest32

import javax.inject.Inject


private object ContractTypeEnum extends Enumeration {
  type ContractType = Value
  val CONTRACT_TOKEN_ISSUE, CONTRACT_SERVICE, CONTRACT_TICKET, CONTRACT_RAFFLE_WAITING_TOKEN,
    CONTRACT_RAFFLE_ACTIVE, CONTRACT_WINNER, CONTRACT_RAFFLE_REDEEM= Value
}
import ContractTypeEnum._

class Addresses @Inject()(client: Client, explorer: Explorer, utils: Utils, contract: RaffleContract){
  private var tokenRepo: ErgoContract = _
  private var service: ErgoContract = _
  private var ticket: ErgoContract = _
  private var raffleWaitingToken: ErgoContract = _
  private var raffleActive: ErgoContract = _
  private var raffleWinner: ErgoContract = _
  private var raffleRedeem: ErgoContract = _

  private def getContractScriptHash(contract: ErgoContract): Digest32 = {
    scorex.crypto.hash.Blake2b256(contract.getErgoTree.bytes)
  }

  private def generateRaffleRedeem(ctx: BlockchainContext): Unit ={
    this.raffleRedeem = ctx.compileContract(ConstantsBuilder.create()
      .item("raffleServiceNFT", ErgoId.create(Configs.token.nft).getBytes)
      .item("raffleServiceToken", ErgoId.create(Configs.token.service).getBytes)
      .build(), contract.raffleRedeemScript)
  }

  private def generateRaffleWinner(ctx:BlockchainContext): Unit ={
    this.raffleWinner = ctx.compileContract(ConstantsBuilder.create()
      .item("raffleServiceNFT", ErgoId.create(Configs.token.nft).getBytes)
      .item("raffleServiceToken", ErgoId.create(Configs.token.service).getBytes)
      .item("fee", Configs.fee)
      .build(), contract.raffleWinnerScript)
  }

  private def generateRaffleActive(ctx:BlockchainContext): Unit ={
    val raffleRedeemScriptHash = getContractScriptHash(this.getContract(CONTRACT_RAFFLE_REDEEM))
    val raffleWinnerScriptHash = getContractScriptHash(this.getContract(CONTRACT_WINNER))
    val ticketScriptHash = getContractScriptHash(this.getContract(CONTRACT_TICKET))
    this.raffleActive = ctx.compileContract(ConstantsBuilder.create()
      .item("winnerScriptHash", raffleWinnerScriptHash)
      .item("ticketScriptHash", ticketScriptHash)
      .item("redeemScriptHash", raffleRedeemScriptHash)
      .item("raffleServiceNFT", ErgoId.create(Configs.token.nft).getBytes)
      .item("raffleServiceToken", ErgoId.create(Configs.token.service).getBytes)
      .item("fee", Configs.fee)
      .build(), contract.raffleActiveScript)
  }

  private def generateRaffleWaitingToken(ctx: BlockchainContext): Unit ={
    val raffleActiveScriptHash = getContractScriptHash(this.getContract(CONTRACT_RAFFLE_ACTIVE))
    this.raffleWaitingToken = ctx.compileContract(ConstantsBuilder.create()
      .item("raffleActiveServiceToken", ErgoId.create(Configs.token.service).getBytes)
      .item("raffleActiveHash", raffleActiveScriptHash)
      .item("fee", Configs.fee)
      .build(), contract.RaffleScriptWaitingToken);
  }

  private def generateTokenRepo(ctx: BlockchainContext): Unit ={
    this.tokenRepo = ctx.compileContract(ConstantsBuilder.create()
      .item("raffleServiceToken", ErgoId.create(Configs.token.service).getBytes)
      .build(), contract.raffleTokenIssueRepo);
  }

  private def generateTicket(ctx: BlockchainContext): Unit ={
    this.ticket = ctx.compileContract(ConstantsBuilder.create()
      .item("raffleServiceToken", ErgoId.create(Configs.token.service).getBytes)
      .item("raffleNFT", ErgoId.create(Configs.token.nft).getBytes)
      .item("fee", Configs.fee)
      .build(), contract.ticketScript);
  }

  private def generateService(ctx: BlockchainContext): Unit = {
    val raffleScriptHash = getContractScriptHash(this.getContract(CONTRACT_RAFFLE_WAITING_TOKEN))
    val tokenIssueScriptHash: Digest32 = getContractScriptHash(this.getContract(CONTRACT_TOKEN_ISSUE))
    this.service = ctx.compileContract(ConstantsBuilder.create()
      .item("raffleScriptHash", raffleScriptHash)
      .item("raffleTokenIssueHash", tokenIssueScriptHash)
      .item("raffleServiceNFT", ErgoId.create(Configs.token.nft).getBytes)
      .item("raffleServiceToken", ErgoId.create(Configs.token.service).getBytes)
      .item("minFee", Configs.fee)
      .build(), contract.RaffleServiceScript)
  }

  private def getContract(contractType: ContractType): ErgoContract ={
    client.getClient.execute((ctx: BlockchainContext) => {
      if(contractType == CONTRACT_TICKET ) {
        if (this.ticket == null) {
          this.generateTicket(ctx)
        }
        this.ticket
      }else if(contractType == CONTRACT_SERVICE) {
        if(this.service == null) {
          this.generateService(ctx)
        }
        this.service
      }else if(contractType == CONTRACT_RAFFLE_ACTIVE) {
        if(this.raffleActive == null) {
          this.generateRaffleActive(ctx)
        }
        this.raffleActive
      }else if(contractType == CONTRACT_RAFFLE_REDEEM) {
        if(this.raffleRedeem == null) {
          this.generateRaffleRedeem(ctx)
        }
        this.raffleRedeem
      }else if(contractType == CONTRACT_WINNER) {
        if(this.raffleWinner == null) {
          this.generateRaffleWinner(ctx)
        }
        this.raffleWinner
      }else if(contractType == CONTRACT_RAFFLE_WAITING_TOKEN) {
        if(this.raffleWaitingToken == null) {
          this.generateRaffleWaitingToken(ctx)
        }
        this.raffleWaitingToken
      }else{
        if(this.tokenRepo == null){
          this.generateTokenRepo(ctx)
        }
        this.tokenRepo
      }
    })
  }

  def getRaffleServiceContract(): ErgoContract = {
    getContract(CONTRACT_SERVICE)
  }

  def getTicketContract(): ErgoContract = {
    getContract(CONTRACT_TICKET)
  }

  def getRaffleTokenIssueContract(): ErgoContract = {
    getContract(CONTRACT_TOKEN_ISSUE)
  }

  def getRaffleWaitingTokenContract(): ErgoContract = {
    getContract(CONTRACT_RAFFLE_WAITING_TOKEN)
  }

  def getRaffleActiveContract(): ErgoContract = {
    getContract(CONTRACT_RAFFLE_ACTIVE)
  }

  def getRaffleWinnerContract(): ErgoContract = {
    getContract(CONTRACT_WINNER)
  }

  def getRaffleRedeemContract(): ErgoContract = {
    getContract(CONTRACT_RAFFLE_REDEEM)
  }

  def getRaffleCreateProxyContract(pk: String, charity: Long, name: String, description: String, deadlineHeight: Long,
                                   charityAddr: String, goal: Long, ticketPrice: Long): String = {
    client.getClient.execute((ctx: BlockchainContext) => {
      val proxyContract = ctx.compileContract(ConstantsBuilder.create()
        .item("PK", Address.create(pk).getPublicKey)
        .item("charityCoef", charity)
        .item("ticketPrice", ticketPrice)
        .item("goal", goal)
        .item("deadlineHeight", deadlineHeight)
        .item("charityAddress", deadlineHeight)
        .item("raffleServiceNFT", ErgoId.create(Configs.token.nft).getBytes)
        .item("raffleServiceToken", ErgoId.create(Configs.token.service).getBytes)
        .item("charityAddress", new ErgoTreeContract(Address.create(charityAddr).getErgoAddress.script).getErgoTree.bytes)
        .build(), contract.createRaffleProxyScript)
      Configs.addressEncoder.fromProposition(proxyContract.getErgoTree).get.toString
    })
  }
}
