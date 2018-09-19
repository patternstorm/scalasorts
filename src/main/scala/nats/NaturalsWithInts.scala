package nats

import universe.Universe._

trait NaturalsWithInts {
  self: Naturals =>

  case class NatAsInt(state: Int)

  implicit object NatAsInt extends (nat as NatAsInt) {
    override def encode(x: nat.rep): NatAsInt = x match {
      case nat.zero.rep => NatAsInt(0)
      case nat.succ.rep(n) => NatAsInt(encode(n).state + 1)
    }

    override def decode(x: NatAsInt): nat.rep = if (x.state == 0) nat.zero.rep else nat.succ.rep(decode(NatAsInt(x.state - 1)))
  }

  implicit object addNatAsInt extends Implementation[nat.add, nat ->: nat ->: nat, NatAsInt => NatAsInt => NatAsInt] {
    override def apply(): NatAsInt => NatAsInt => NatAsInt = x => y => NatAsInt(x.state + y.state)
  }
}
