package banking.accounts


import org.scalatest.{FunSpec, GivenWhenThen, Matchers}
import universe.Universe._
import banking.accounts.ACCOUNT._
import nats.NAT._
import nats.{NATasInt, NATasLong}

class AccountsSpec extends FunSpec with Matchers with GivenWhenThen {

  describe("the Sort `account`" +
    "in a context where two different representations of the sort, `SavingsAccount` and `InvestmentAccount` exist in scope") {

    it("can be created as a `SavingsAccount` or as an `InvestmentAccount` ") {
      type SavingsAccount = SavingsAccount.type
      type InvestmentAccount = InvestmentAccount.type
      val sacc1: Rep[SavingsAccount] = account.create()
      sacc1.value shouldEqual SavingsAccount(NATasInt(0))
      val iacc1: Rep[InvestmentAccount] = account.create()
      iacc1.value shouldEqual InvestmentAccount(NATasLong(0))
    }
    it("allows to deposit money") {
      type SavingsAccount = SavingsAccount.type
      type InvestmentAccount = InvestmentAccount.type
      val sacc1: Rep[SavingsAccount] = account.create()
      val iacc1: Rep[InvestmentAccount] = account.create()
      val sacc2: Rep[SavingsAccount] = account.deposit(account.create(), nat.succ(nat.zero()))
      sacc2.value shouldEqual SavingsAccount(NATasInt(1))
      val sacc3: Rep[SavingsAccount] = account.deposit(sacc1, nat.succ(nat.zero()))
      sacc3.value shouldEqual SavingsAccount(NATasInt(1))
      val sacc4: Rep[SavingsAccount] = account.deposit(iacc1, nat.succ(nat.zero()))
      sacc4.value shouldEqual SavingsAccount(NATasInt(1))
      val iacc2: Rep[InvestmentAccount] = account.deposit(account.create(), nat.succ(nat.zero()))
      iacc2.value shouldEqual InvestmentAccount(NATasLong(1))
      val iacc3: Rep[InvestmentAccount] = account.deposit(sacc1, nat.succ(nat.zero()))
      iacc3.value shouldEqual InvestmentAccount(NATasLong(1))
      val iacc4: Rep[InvestmentAccount] = account.deposit(iacc1, nat.succ(nat.zero()))
      iacc4.value shouldEqual InvestmentAccount(NATasLong(1))
    }
    it("allows to consult the amount of money it holds") {
      type SavingsAccount = SavingsAccount.type
      type InvestmentAccount = InvestmentAccount.type
      type NATasInt = NATasInt.type
      type NATasLong = NATasLong.type
      val sacc1: Rep[SavingsAccount] = account.create()
      val iacc1: Rep[InvestmentAccount] = account.create()
      val sbalance1: Rep[NATasInt] = account.balance(sacc1)
      sbalance1.value shouldEqual NATasInt(0)
      val sbalance2: Rep[NATasInt] = account.balance(iacc1)
      sbalance2.value shouldEqual NATasInt(0)
      val ibalance1: Rep[NATasLong] = account.balance(sacc1)
      ibalance1.value shouldEqual NATasLong(0)
      val ibalance2: Rep[NATasLong] = account.balance(iacc1)
      ibalance2.value shouldEqual NATasLong(0)
    }
  }
}
