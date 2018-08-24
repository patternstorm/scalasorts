
import ints.{Integers, IntegersWithLongs}
import nats.Naturals
import nats.NaturalsWithInts
import universe.Universe
import org.scalatest.{FunSpec, GivenWhenThen, Matchers}



class NaturalsIntegersSpec extends FunSpec with Matchers with GivenWhenThen
with Universe with Naturals with NaturalsWithInts with Integers with IntegersWithLongs {

  describe("..") {
    it("...") {
      implicitly[nothing]
      implicitly[NAT.sort]

      implicitly[NAT.zero]
      implicitly[Morphism[NAT.zero, nothing ->: NAT.sort]]

      implicitly[NAT.succ]
      implicitly[Morphism[NAT.succ, NAT.sort ->: NAT.sort]]
      implicitly[Morphism[NAT.succ ∙ NAT.zero, nothing ->: NAT.sort]]
      implicitly[Morphism[(NAT.succ ∙ (NAT.succ ∙ NAT.zero)), nothing ->: NAT.sort]]


      implicitly[NAT.add]
      implicitly[Morphism[NAT.add, NAT.sort ->: NAT.sort ->: NAT.sort]]
      implicitly[Morphism[NAT.add ∙ (NAT.succ ∙ NAT.zero), NAT.sort ->: NAT.sort]]
      val z = implicitly[Morphism[(NAT.add ∙ (NAT.succ ∙ NAT.zero)) ∙ NAT.zero, nothing ->: NAT.sort]]
      z().state shouldEqual 1
      val y = implicitly[Morphism[NAT.succ ∙ ((NAT.add ∙ (NAT.succ ∙ NAT.zero)) ∙ NAT.zero), nothing ->: NAT.sort]]
      y().state shouldEqual 2
      val x = implicitly[Morphism[(NAT.add ∙ (NAT.succ ∙ NAT.zero)) ∙ (NAT.succ ∙ ((NAT.add ∙ (NAT.succ ∙ NAT.zero)) ∙ NAT.zero)), nothing ->: NAT.sort]]
      val rep: NAT.sort#rep = x()
      rep.state shouldEqual 3
      //implicitly[(NAT.succ ∙ NAT.succ) ->: NAT.nat]

      val term1 : NAT.succ ∙ NAT.zero = implicitly[NAT.succ ∙ NAT.zero]
      val term2 : (NAT.add ∙ (NAT.succ ∙ NAT.zero)) ∙ (NAT.succ ∙ ((NAT.add ∙ (NAT.succ ∙ NAT.zero)) ∙ NAT.zero)) = implicitly[(NAT.add ∙ (NAT.succ ∙ NAT.zero)) ∙ (NAT.succ ∙ ((NAT.add ∙ (NAT.succ ∙ NAT.zero)) ∙ NAT.zero))]
      //val term2 = implicitly[NAT.succ ∙ NAT.succ]


      val n  = NAT.add(NAT.succ(NAT.zero()),NAT.succ(NAT.zero()))
      val w: Int = n()
      w shouldEqual 2

      val l  = INT.add(INT.succ(INT.zero()),INT.succ(INT.zero()))
      val v: Long = l()
      v shouldEqual 2L

      import NAT._
      val m = add(succ(zero()),succ(zero()))
      m().state shouldEqual 2

      val s =implicitly[Morphism[NAT.succ, NAT.sort ->: NAT.sort]]
      s(n()).state shouldEqual 3
      val k = succ(n)
      k().state shouldEqual 3

      implicitly[ (NAT.add ∙ (NAT.succ ∙ NAT.zero) ∙ NAT.zero) ⟿ (NAT.succ ∙ NAT.zero)]
      //implicitly[ (NAT.add ∙ (NAT.succ ∙ NAT.zero) ∙ (NAT.succ ∙ NAT.zero)) ⟿ (NAT.succ ∙ NAT.zero)]

      NAT.add

      NAT.add.hashCode() should not equal NAT.succ.hashCode()

    }
  }
}
