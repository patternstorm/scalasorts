package banking.accounts


import org.scalatest.{FunSpec, GivenWhenThen, Matchers}
import universe.Universe._
import banking.accounts.ACCOUNT._
import nats.NAT._
import nats.NATasInt
import banking.accounts.ACCOUNTasBalance._

class AccountsSpec extends FunSpec with Matchers with GivenWhenThen {

  describe("...") {
    it("...") {
      val m = account.deposit(account.create, nat.succ(nat.zero()))
      val n = account.balance(m)
      n() shouldEqual nat.succ.rep(nat.zero.rep)
      //val acc: ACCOUNTasBalance = m()
      //acc.balance.state shouldEqual 1

      val acc: ACCOUNTasBalance = account.create()
      val z: ACCOUNTasBalance = account.deposit(acc, NATasInt(0))
      z shouldEqual ACCOUNTasBalance.create()
      //val b: nat.rep = account.balance(z)
      //shouldEqual nat.zero.rep
    }
  }
}
