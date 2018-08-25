package nats

import universe.Universe

trait NaturalsWithLongs {
    self: Universe with Naturals =>

    case class NATasLong(state: Long)
    object NATasLong { implicit def asLong(x: NATasLong): Long = x.state }

    object NAT extends NAT {
      override type sort = nat.type
      implicit object nat extends Universal {
        override type rep = NATasLong
      }
      override val sort : sort = nat

      implicit object zeroImp extends Morphism[zero, nat] {
        override def apply(n: nothing#rep): NATasLong = NATasLong(0)
      }

      implicit object succImp extends (succ :: nat ->: nat) {
        override def apply(n: nothing#rep): NATasLong => NATasLong = x => NATasLong(x.state + 1)
      }

      implicit object addImp extends (add :: nat ->: nat ->: nat) {
        override def apply(n: nothing#rep): NATasLong => NATasLong => NATasLong = x => y => NATasLong(x.state + y.state)
      }
    }
}
