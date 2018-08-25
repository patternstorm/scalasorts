package nats

import universe.Universe

trait NaturalsWithInts {
    self: Universe with Naturals =>

    case class NATasInt(state: Int)
    object NATasInt { implicit def asInt(x: NATasInt): Int = x.state }

    object NAT extends NAT {
      override type sort = nat.type
      implicit object nat extends Universal {
        override type rep = NATasInt
      }
      override val sort : sort = nat

      implicit object zeroImp extends (zero :: nat) {
        override def apply(n: nothing#rep): NATasInt = NATasInt(0)
      }

      implicit object succImp extends (succ :: nat ->: nat) {
        override def apply(n: nothing#rep): NATasInt => NATasInt = x => NATasInt(x.state + 1)
      }

      implicit object addImp extends (add :: nat ->: nat ->: nat) {
        override def apply(n: nothing#rep): NATasInt => NATasInt => NATasInt = x => y => NATasInt(x.state + y.state)
      }
    }
}
