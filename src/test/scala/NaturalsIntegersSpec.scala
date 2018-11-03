

import ints.INTasLong
import nats.NATasInt
import org.scalatest.{FunSpec, GivenWhenThen, Matchers}
import universe.Universe._
import nats.NAT._
import ints.INT._


class NaturalsIntegersSpec extends FunSpec with Matchers with GivenWhenThen {


  describe("..") {
    it("...") {
      implicitly[nat.type as NATasInt]
      implicitly[int.type as INTasLong]

      var valnat: NATasInt = nat.zero.rep
      val repnat: nat.rep = valnat
      var valint: INTasLong = int.zero.rep
      val repint: int.rep = valint


      val t0 = nat.succ(nat.zero())
      valnat = t0.as[NATasInt]
      valnat.state shouldEqual 1

      val t1 = nat.add(nat.succ(nat.zero()), nat.succ(nat.zero()))
      valnat = t1.as[NATasInt]
      valnat.state shouldEqual 2

      val t2 = int.add(int.succ(int.zero()), int.succ(int.zero()))
      valint = t2.as[INTasLong]
      valint shouldEqual INTasLong(2L)

      import nats.NAT.nat._
      val t3 = add(succ(zero()), succ(zero()))
      valnat = t3.as[NATasInt]
      valnat.state shouldEqual 2

      val t4 = succ(t3)
      valnat = t4.as[NATasInt]
      valnat.state shouldEqual 3


      nat.add

      nat.add.hashCode() should not equal nat.succ.hashCode()

    }
  }
}
