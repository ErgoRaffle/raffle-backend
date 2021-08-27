package models


case class CreateReq(id: Long, name: String, description: String, goal: Long,
                     deadlineHeight: Long, charityPercent: Int, charityAddr: String,
                     ticketPrice: Long, state: Int, walletAddress: String, paymentAddress: String,
                     createTxId: Option[String],  mergeTxId: Option[String], timeOut: Long,
                     ttl: Long, deleted: Boolean)


case class DonateReq(id: Long, ticketCount: Long, fee: Long, raffleDeadline: Long , state: Int, paymentAddress: String,
                     raffleAddress: String, raffleToken: String, donateTxID: Option[String],
                     participantAddress: String, timeOut: Long, ttl: Long, deleted: Boolean)

