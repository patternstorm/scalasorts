package nats

import universe.Universe

trait NaturalsWithInts {
    self: Universe with Naturals =>

    case class NATasInt(state: Int)
    object NATasInt { implicit def asInt(x: NATasInt): Int = x.state }

    object NAT extends NAT {
      override type sort = nat.type
      implicit object nat extends Universal {
        override type Rep = NATasInt
      }
      override val sort : sort = nat
      override private[nats] def zeroImp(x: Nothing#Rep): NATasInt = NATasInt(0)
      override private[nats] def succImp(x: NATasInt): NATasInt = NATasInt(x.state + 1)
      override private[nats] def addImp(x: NATasInt, y: NATasInt): NATasInt = NATasInt(x.state + y.state)
    }

}
