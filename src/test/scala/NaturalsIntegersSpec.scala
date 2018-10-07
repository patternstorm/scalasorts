

import ints.INTasLong
import nats.NATasInt
import org.scalatest.{FunSpec, GivenWhenThen, Matchers}
import universe.Universe._


class NaturalsIntegersSpec extends FunSpec with Matchers with GivenWhenThen {


  describe("..") {
    it("...") {
      implicitly[nat.type as NATasInt]
      implicitly[int.type as INTasLong]
      var valnat: NATasInt = nat.zero.rep
      val repnat: nat.rep = valnat
      var valint: INTasLong = int.zero.rep
      val repint: int.rep = valint
      //      implicitly[nothing]
      //      implicitly[nat]
      //      implicitly[(nat ->: nat) <-> (NatAsInt => NatAsInt)]
      //      implicitly[(nat `,` nat) <-> (NatAsInt, NatAsInt)]
      //      implicitly[((nat `,` nat) ->: nat)#Domain <-> (NatAsInt, NatAsInt)]
      //      implicitly[((nat `,` nat) ->: nat)#Image <-> NatAsInt]
      //      val h : ((nat `,` nat) ->: nat) <-> (((NatAsInt, NatAsInt)) => NatAsInt) = <->.arrowRep[nat `,` nat, nat, (NatAsInt, NatAsInt), NatAsInt]
      //      implicitly[((nat `,` nat) ->: nat) <-> (((NatAsInt, NatAsInt)) => NatAsInt)]
      //      implicitly[nat =:= nat.self]
      //
      //      implicitly[nat.zero]
      //      implicitly[Morphism[nat.zero, nat]]
      //
      //      implicitly[nat.succ]
      //      implicitly[Morphism[nat.succ, nat ->: nat]]
      //      implicitly[Morphism[nat.succ ∙ nat.zero, nat]]
      //      implicitly[Morphism[(nat.succ ∙ (nat.succ ∙ nat.zero)), nat]]
      //
      //      implicitly[(int ->: int) <-> (IntAsLong => IntAsLong)]
      //
      //
      //      implicitly[nat.add]
      //      implicitly[Morphism[nat.add, nat ->: nat ->: nat]]
      //      val sig = implicitly[(nat `,` nat) ->: nat]
      //      println(sig)
      //      implicitly[Morphism[nat.add ∙ (nat.succ ∙ nat.zero), nat ->: nat]]
      //      val z = implicitly[Morphism[(nat.add ∙ (nat.succ ∙ nat.zero)) ∙ nat.zero, nat]]
      //      println(z)
      //      var value: NatAsInt = z()
      //      value.state shouldEqual 1
      //      val y = implicitly[Morphism[nat.succ ∙ ((nat.add ∙ (nat.succ ∙ nat.zero)) ∙ nat.zero), nat]]
      //      value = y()
      //      value.state shouldEqual 2
      //      val x = implicitly[Morphism[(nat.add ∙ (nat.succ ∙ nat.zero)) ∙ (nat.succ ∙ ((nat.add ∙ (nat.succ ∙ nat.zero)) ∙ nat.zero)), nat]]
      //      value = x()
      //      value.state shouldEqual 3
      //      //implicitly[(nat.succ ∙ nat.succ) ->: nat.nat]
      //
      //      val term1: nat.succ ∙ nat.zero = implicitly[nat.succ ∙ nat.zero]
      //      val term2: (nat.add ∙ (nat.succ ∙ nat.zero)) ∙ (nat.succ ∙ ((nat.add ∙ (nat.succ ∙ nat.zero)) ∙ nat.zero)) = implicitly[(nat.add ∙ (nat.succ ∙ nat.zero)) ∙ (nat.succ ∙ ((nat.add ∙ (nat.succ ∙ nat.zero)) ∙ nat.zero))]
      //      //val term2 = implicitly[nat.succ ∙ nat.succ]



      val n = nat.add(nat.succ(nat.zero()), nat.succ(nat.zero()))
      println(n)
      //implicitly[Implementation[nat.zero, nat, NatAsInt]]
      // implicitly[Implementation[(nat.add ∙ nat.zero) ∙ nat.zero, nat, NatAsInt]]
      //val imp = implicitly[Implementation[(nat.add ∙ (nat.succ ∙ nat.zero)) ∙ nat.zero, nat, NatAsInt]]
      // println(imp)
      // implicitly[Implementation[nat.succ ∙ nat.zero, nat, NatAsInt]]
      valnat = n()
      valnat.state shouldEqual 2

      val l = int.add(int.succ(int.zero()), int.succ(int.zero()))
      //implicitly[Implementation[int.zero, int, IntAsLong]]
      //implicitly[Implementation[int.succ ∙ int.zero, int, IntAsLong]]
      //implicitly[Implementation[(int.add ∙ int.zero) ∙ int.zero, int, IntAsLong]]
      valint = l()
      valint shouldEqual INTasLong(2L)
      val m = add(succ(zero()), succ(zero()))
      var v2: NATasInt = m()
      v2.state shouldEqual 2

      //      val s = implicitly[Morphism[nat.succ, nat ->: nat]]
      //      v2 = s()(n())
      //      v2.state shouldEqual 3
      val k = succ(n)
      v2 = k()
      v2.state shouldEqual 3

      //implicitly[ (nat.add ∙ (nat.succ ∙ nat.zero) ∙ nat.zero) ⟿ (nat.succ ∙ nat.zero)]
      //implicitly[ (nat.add ∙ (nat.succ ∙ nat.zero) ∙ (nat.succ ∙ nat.zero)) ⟿ (nat.succ ∙ nat.zero)]

      nat.add

      nat.add.hashCode() should not equal nat.succ.hashCode()

    }
  }
}
