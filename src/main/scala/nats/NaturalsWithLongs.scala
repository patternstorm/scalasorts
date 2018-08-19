package nats

import universe.Universe

trait NaturalsWithLongs {
    self: Universe with Naturals =>

    case class NATasLong(state: Long)
    object NATasLong { implicit def asLong(x: NATasLong): Long = x.state }

    object NAT extends NAT {
      override type sort = nat.type
      implicit object nat extends Universal {
        override type Rep = NATasLong
      }
      override val sort : sort = nat
      override private[nats] def zeroImp(x: Nothing#Rep): NATasLong = NATasLong(0)
      override private[nats] def succImp(x: NATasLong): NATasLong = NATasLong(x.state + 1)
      override private[nats] def addImp(x: NATasLong, y: NATasLong): NATasLong = NATasLong(x.state + y.state)
    }
}
