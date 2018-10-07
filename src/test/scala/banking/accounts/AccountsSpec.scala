package banking.accounts


import org.scalatest.{FunSpec, GivenWhenThen, Matchers}
import universe.Universe._
import banking.accounts.ACCOUNT._
import nats.NAT._

class AccountsSpec extends FunSpec with Matchers with GivenWhenThen {

  describe("...") {
    it("...") {
      val m = account.deposit(account.create, nat.succ(nat.zero))
      val acc: ACCOUNTasBalance = m()
      acc.balance.state shouldEqual 1
    }
  }
}
