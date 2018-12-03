

import ints.INTasLong
import nats.NATasInt
import nats.NATasIntImp
import org.scalatest.{FunSpec, GivenWhenThen, Matchers}
import universe.Universe._
import nats.NAT._
import ints.INT._

import scala.collection.concurrent.TrieMap


class NaturalsIntegersSpec extends FunSpec with Matchers with GivenWhenThen {

  describe("..") {
    it("...") {
      type NATasInt = NATasInt.type
      type INTasLong = INTasLong.type
      implicitly[NATasInt]
      implicitly[NATasInt <:< Representation]
      implicitly[NATasInt reps nat.type]
      val nat1: Rep[NATasInt] = nat.zero()
      nat1.state shouldEqual 0
      val nat2: Rep[NATasInt] = NATasInt(0)
      nat2.state shouldEqual 0


      implicitly[nat.succ.type ∙ nat.zero.type]
      implicitly[(nat.succ.type ∙ nat.zero.type) :: nat.type]
      val nat3: Rep[NATasInt] = nat.succ(nat.zero())
      val nat4: Rep[NATasInt] = nat.succ(nat3)
      val nat5: Rep[NATasInt] = nat.succ(nat4)
      nat5.state shouldEqual 3


      implicitly[nat.add.type ∙ (nat.succ.type ∙ nat.zero.type) ∙ nat.zero.type]
      implicitly[nat.add.type ∙ (nat.succ.type ∙ nat.zero.type) ∙ (nat.succ.type ∙ nat.zero.type)]
      implicitly[((nat.add.type ∙ (nat.succ.type ∙ nat.zero.type)) ∙ (nat.zero.type)) :: nat.type]
      implicitly[((nat.add.type ∙ (nat.zero.type)) ∙ (nat.succ.type ∙ nat.zero.type)) :: nat.type]
      val nat6: Rep[NATasInt] = nat.add(nat.succ(nat.zero()), nat.succ(nat.zero()))
      nat6.state shouldEqual 2

      val int1: Rep[INTasLong] = int.add(int.succ(int.zero()), int.succ(int.zero()))
      int1.value shouldEqual INTasLong(2L)

      import nats.NAT.nat._
      val nat7: Rep[NATasInt] = add(succ(zero()), succ(zero()))
      nat7.state shouldEqual 2

      val nat8: Rep[NATasInt] = succ(nat7)
      nat8.state shouldEqual 3


      nat.add

      nat.add.hashCode() should not equal nat.succ.hashCode()

    }
  }
}
