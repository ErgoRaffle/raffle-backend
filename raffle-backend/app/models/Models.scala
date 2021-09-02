package models


case class CreateReq(id: Long, name: String, description: String, goal: Long,
                     deadlineHeight: Long, charityPercent: Int, charityAddr: String,
                     ticketPrice: Long, state: Int, walletAddress: String, paymentAddress: String,
                     createTxId: Option[String],  mergeTxId: Option[String], timeStamp: String,
                     ttl: Long, deleted: Boolean)


case class DonateReq(id: Long, ticketCount: Long, fee: Long, raffleDeadline: Long , state: Int, paymentAddress: String,
                     raffleToken: String, donateTxID: Option[String], participantAddress: String,
                     timeStamp: String, ttl: Long, deleted: Boolean)

