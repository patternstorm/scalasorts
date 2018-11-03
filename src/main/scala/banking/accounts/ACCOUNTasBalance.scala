package banking.accounts


import nats.NATasInt
import universe.Universe._
import ACCOUNT._
import nats.NAT._

case class ACCOUNTasBalance(balance: NATasInt)

object ACCOUNTasBalance extends account.create.impl[ACCOUNTasBalance]
  with account.deposit.impl[ACCOUNTasBalance, NATasInt, ACCOUNTasBalance] {
  override def create(): ACCOUNTasBalance = ACCOUNTasBalance(NATasInt(0))

  //TODO: ACCOUNTasBalance(nat.add(x._1.balance,x._2))
  override def deposit(account: ACCOUNTasBalance, amount: NATasInt): ACCOUNTasBalance = ACCOUNTasBalance(NATasInt(account.balance.state + amount.state))
}
