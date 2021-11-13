package raffle

import helpers.{Configs, Utils}
import network.{Client, Explorer}
import org.ergoplatform.appkit.{Address, BlockchainContext, ConstantsBuilder, ErgoContract, ErgoId}
import scorex.crypto.hash.Digest32

import javax.inject.Inject


class Addresses @Inject()(client: Client, contract: RaffleContract){
  private lazy val tokenRepo: ErgoContract = generateTokenRepo()
  private lazy val service: ErgoContract = generateService()
  private lazy val ticket: ErgoContract = generateTicket()
  private lazy val raffleInactive: ErgoContract = generateRaffleWaitingToken()
  private lazy val raffleActive: ErgoContract = generateRaffleActive()
  private lazy val raffleWinner: ErgoContract = generateRaffleWinner()
  private lazy val raffleRedeem: ErgoContract = generateRaffleRedeem()

  lazy val tokenRepoAddress: Address = generateAddress(tokenRepo)
  lazy val serviceAddress: Address = generateAddress(service)
  lazy val ticketAddress: Address = generateAddress(ticket)
  lazy val raffleInactiveAddress: Address = generateAddress(raffleInactive)
  lazy val raffleActiveAddress: Address = generateAddress(raffleActive)
  lazy val raffleWinnerAddress: Address = generateAddress(raffleWinner)
  lazy val raffleRedeemAddress: Address = generateAddress(raffleRedeem)

  private def generateAddress(contract: ErgoContract): Address ={
    Address.create(Configs.addressEncoder.fromProposition(contract.getErgoTree).get.toString)
  }
  private def getContractScriptHash(contract: ErgoContract): Digest32 = {
    scorex.crypto.hash.Blake2b256(contract.getErgoTree.bytes)
  }

  private def generateRaffleRedeem(): ErgoContract ={
    client.getClient.execute((ctx: BlockchainContext) => {
      ctx.compileContract(ConstantsBuilder.create()
        .item("raffleServiceNFT", ErgoId.create(Configs.token.nft).getBytes)
        .item("raffleServiceToken", ErgoId.create(Configs.token.service).getBytes)
        .build(), contract.raffleRedeemScript)
    })
  }

  private def generateRaffleWinner(): ErgoContract ={
    client.getClient.execute((ctx: BlockchainContext) => {
      ctx.compileContract(ConstantsBuilder.create()
        .item("raffleServiceNFT", ErgoId.create(Configs.token.nft).getBytes)
        .item("raffleServiceToken", ErgoId.create(Configs.token.service).getBytes)
        .item("fee", Configs.fee)
        .build(), contract.raffleWinnerScript)
    })
  }

  private def generateRaffleActive(): ErgoContract ={
    client.getClient.execute((ctx: BlockchainContext) => {
      val raffleRedeemScriptHash = getContractScriptHash(raffleRedeem)
      val raffleWinnerScriptHash = getContractScriptHash(raffleWinner)
      val ticketScriptHash = getContractScriptHash(ticket)
      ctx.compileContract(ConstantsBuilder.create()
        .item("winnerScriptHash", raffleWinnerScriptHash)
        .item("ticketScriptHash", ticketScriptHash)
        .item("redeemScriptHash", raffleRedeemScriptHash)
        .item("raffleServiceNFT", ErgoId.create(Configs.token.nft).getBytes)
        .item("randomBoxToken", ErgoId.create(Configs.token.oracle).getBytes)
        .item("raffleServiceToken", ErgoId.create(Configs.token.service).getBytes)
        .item("fee", Configs.fee)
        .build(), contract.raffleActiveScript)
    })
  }

  private def generateRaffleWaitingToken(): ErgoContract ={
    client.getClient.execute((ctx: BlockchainContext) => {
      val raffleActiveScriptHash = getContractScriptHash(raffleActive)
      ctx.compileContract(ConstantsBuilder.create()
        .item("raffleActiveServiceToken", ErgoId.create(Configs.token.service).getBytes)
        .item("raffleActiveHash", raffleActiveScriptHash)
        .item("fee", Configs.fee)
        .build(), contract.RaffleScriptWaitingToken)
    })
  }

  private def generateTokenRepo(): ErgoContract ={
    client.getClient.execute((ctx: BlockchainContext) => {
      ctx.compileContract(ConstantsBuilder.create()
        .item("raffleServiceToken", ErgoId.create(Configs.token.service).getBytes)
        .build(), contract.raffleTokenIssueRepo)
    })
  }

  private def generateTicket(): ErgoContract ={
    client.getClient.execute((ctx: BlockchainContext) => {
      ctx.compileContract(ConstantsBuilder.create()
        .item("raffleServiceToken", ErgoId.create(Configs.token.service).getBytes)
        .item("raffleNFT", ErgoId.create(Configs.token.nft).getBytes)
        .item("fee", Configs.fee)
        .item("ExpireHeight", 100) // TODO
        .item("ownerPk", Configs.serviceOwner.getPublicKey)
        .build(), contract.ticketScript)
    })
  }

  private def generateService(): ErgoContract = {
    client.getClient.execute((ctx: BlockchainContext) => {
      val raffleScriptHash = getContractScriptHash(raffleInactive)
      val tokenIssueScriptHash: Digest32 = getContractScriptHash(tokenRepo)
      ctx.compileContract(ConstantsBuilder.create()
        .item("raffleScriptHash", raffleScriptHash)
        .item("raffleTokenIssueHash", tokenIssueScriptHash)
        .item("raffleServiceNFT", ErgoId.create(Configs.token.nft).getBytes)
        .item("raffleServiceToken", ErgoId.create(Configs.token.service).getBytes)
        .item("ownerPK", Configs.serviceOwner.getPublicKey)
        .item("minFee", Configs.fee)
        .build(), contract.RaffleServiceScript)
    })
  }

  // TODO remove these
  def getRaffleServiceContract(): ErgoContract = {
    service
  }

  def getTicketContract(): ErgoContract = {
    ticket
  }

  def getRaffleTokenIssueContract(): ErgoContract = {
    tokenRepo
  }

  def getRaffleWaitingTokenContract(): ErgoContract = {
    raffleInactive
  }

  def getRaffleActiveContract(): ErgoContract = {
    raffleActive
  }

  def getRaffleWinnerContract(): ErgoContract = {
    raffleWinner
  }

  def getRaffleRedeemContract(): ErgoContract = {
    raffleRedeem
  }

  def getRaffleCreateProxyContract(pk: String, charity: Long, name: String, description: String, deadlineHeight: Long,
                                   charityAddr: String, goal: Long, ticketPrice: Long, picLinks: List[String]): String = {
    client.getClient.execute((ctx: BlockchainContext) => {
      var pictureConstraints: String = ""
      for(i <- picLinks.indices){
        pictureConstraints += s"OUTPUTS(1).R6[Coll[Coll[Byte]]].get(${i+2}) == link$i,\n"
      }
      val updateContract = contract.createRaffleProxyScript.format(pictureConstraints)
      val constants = ConstantsBuilder.create()
        .item("userAddress", Address.create(pk).getErgoAddress.script.bytes)
        .item("minFee", Configs.fee)
        .item("refundHeightThreshold", ctx.getHeight + ((Configs.creationDelay / 60 / 2) + 1).toLong)
        .item("charityCoef", charity)
        .item("ticketPrice", ticketPrice)
        .item("goal", goal)
        .item("deadlineHeight", deadlineHeight)
        .item("raffleServiceNFT", ErgoId.create(Configs.token.nft).getBytes)
        .item("raffleServiceToken", ErgoId.create(Configs.token.service).getBytes)
        .item("charityAddress", Address.create(charityAddr).getErgoAddress.script.bytes)
        .item("name", name.getBytes("utf-8"))
        .item("description", description.getBytes("utf-8"))
        .item("maxFee", Configs.fee)
      for(i <- picLinks.indices){
        constants.item(s"link$i", picLinks(i).getBytes("utf-8"))
      }
      val proxyContract = ctx.compileContract(constants.build(), updateContract)
      Configs.addressEncoder.fromProposition(proxyContract.getErgoTree).get.toString
    })
  }
}
