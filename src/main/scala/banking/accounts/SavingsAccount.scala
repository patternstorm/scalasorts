package banking.accounts


import nats.NATasInt
import universe.Universe._
import ACCOUNT._
import nats.NAT._


object SavingsAccount extends account.impl {

  case class rep(balance: NATasInt.rep) {
    override def toString: String = symbol + "(balance=" + balance + ")"
  }

  override val symbol: String = "SavingsAccount"
  implicit val me: SavingsAccount.type = this

  implicit def unlift(a: Rep[SavingsAccount.type]): rep = a.value

  override protected def _decode(x: rep): account.type#rep =
    if (x.balance.state == 0) account.create.rep else account.deposit.rep(account.create.rep, NATasInt._decode(x.balance))

  override protected def _encode(x: account.create.rep): rep = SavingsAccountImp.create()

  override protected def _encode(x: account.deposit.rep): rep = SavingsAccountImp.deposit(x._1, x._2)

  def apply(balance: NATasInt.rep): rep = rep(balance)
}

object SavingsAccountImp extends
  account.create.impl[SavingsAccount.type] with
  account.deposit.impl[SavingsAccount.type, NATasInt.type, SavingsAccount.type] with
  account.balance.impl[SavingsAccount.type, NATasInt.type] {
  override def create(): SavingsAccount.rep = SavingsAccount(NATasInt(0))

  //TODO: ACCOUNTasBalance(nat.add(x._1.balance,x._2))
  override def deposit(account: SavingsAccount.rep, amount: NATasInt.rep): SavingsAccount.rep = SavingsAccount(NATasInt(account.balance.state + amount.state))

  override def balance(account: SavingsAccount.rep): NATasInt.rep = account.balance
}
