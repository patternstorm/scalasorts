package banking.accounts


import nats.NATasInt
import universe.Universe._
import ACCOUNT._
import nats.NAT._

case class ACCOUNTasBalance(balance: NATasInt)

object ACCOUNTasBalance extends account.imp[ACCOUNTasBalance] {
  implicitly[nat.sort as NATasInt]

  override def _encode(x: account.create.rep): ACCOUNTasBalance = ACCOUNTasBalance(NATasInt(0))

  override def _encode(x: account.deposit.rep): ACCOUNTasBalance = {
    //TODO: ACCOUNTasBalance(nat.add(x._1.balance,x._2))
    val amount: NATasInt = x._2
    val account: ACCOUNTasBalance = x._1
    ACCOUNTasBalance(NATasInt(account.balance.state + amount.state))
  }

  override def _decode(acc: ACCOUNTasBalance): account.rep = if (acc.balance == NATasInt(0)) account.create.rep
  else account.deposit.rep(account.create.rep, acc.balance)
}
