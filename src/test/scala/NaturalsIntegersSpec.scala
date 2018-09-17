
import ints.{Integers, IntegersWithLongs}
import nats.{Naturals, NaturalsWithInts}
import org.scalatest.{FunSpec, GivenWhenThen, Matchers}
import universe.Universe._



class NaturalsIntegersSpec extends FunSpec with Matchers with GivenWhenThen
  with Naturals with NaturalsWithInts with Integers with IntegersWithLongs {
  
  describe("..") {
    it("...") {
      implicitly[nothing]
      implicitly[nat]

      implicitly[nat.zero]
      implicitly[Morphism[nat.zero, nat]]

      implicitly[nat.succ]
      implicitly[Morphism[nat.succ, nat ->: nat]]
      implicitly[Morphism[nat.succ ∙ nat.zero, nat]]
      implicitly[Morphism[(nat.succ ∙ (nat.succ ∙ nat.zero)), nat]]


      implicitly[nat.add]
      implicitly[Morphism[nat.add, nat ->: nat ->: nat]]
      implicitly[Morphism[nat.add ∙ (nat.succ ∙ nat.zero), nat ->: nat]]
      val z = implicitly[Morphism[(nat.add ∙ (nat.succ ∙ nat.zero)) ∙ nat.zero, nat]]
      var value: NatAsInt = z()
      value.state shouldEqual 1
      val y = implicitly[Morphism[nat.succ ∙ ((nat.add ∙ (nat.succ ∙ nat.zero)) ∙ nat.zero), nat]]
      value = y()
      value.state shouldEqual 2
      val x = implicitly[Morphism[(nat.add ∙ (nat.succ ∙ nat.zero)) ∙ (nat.succ ∙ ((nat.add ∙ (nat.succ ∙ nat.zero)) ∙ nat.zero)), nat]]
      value = x()
      value.state shouldEqual 3
      //implicitly[(nat.succ ∙ nat.succ) ->: nat.nat]

      val term1: nat.succ ∙ nat.zero = implicitly[nat.succ ∙ nat.zero]
      val term2: (nat.add ∙ (nat.succ ∙ nat.zero)) ∙ (nat.succ ∙ ((nat.add ∙ (nat.succ ∙ nat.zero)) ∙ nat.zero)) = implicitly[(nat.add ∙ (nat.succ ∙ nat.zero)) ∙ (nat.succ ∙ ((nat.add ∙ (nat.succ ∙ nat.zero)) ∙ nat.zero))]
      //val term2 = implicitly[nat.succ ∙ nat.succ]


      val n = nat.add(nat.succ(nat.zero()), nat.succ(nat.zero()))
      value = n()
      value.state shouldEqual 2

      val l = int.add(int.succ(int.zero()), int.succ(int.zero()))
      var v: IntAsLong = l()
      v shouldEqual 2L

      import nat._
      val m = add(succ(zero()),succ(zero()))
      m().state shouldEqual 2

      val s = implicitly[Morphism[nat.succ, nat ->: nat]]
      s()(n()).state shouldEqual 3
      val k = succ(n)
      k().state shouldEqual 3

      //implicitly[ (nat.add ∙ (nat.succ ∙ nat.zero) ∙ nat.zero) ⟿ (nat.succ ∙ nat.zero)]
      //implicitly[ (nat.add ∙ (nat.succ ∙ nat.zero) ∙ (nat.succ ∙ nat.zero)) ⟿ (nat.succ ∙ nat.zero)]

      nat.add

      nat.add.hashCode() should not equal nat.succ.hashCode()

    }
  }
}
