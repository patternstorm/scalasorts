package nats

import universe.Universe._

case class NATasInt(state: Int)

object NATasInt extends nat.imp[NATasInt] {
  override def _encode(x: nat.zero.rep): NATasInt = NATasInt(0)

  override def _encode(x: nat.succ.rep): NATasInt = NATasInt(1 + asImp(x._1).state)

  override def _decode(x: NATasInt): nat.rep = if (x.state == 0) nat.zero.rep else nat.succ.rep(_decode(NATasInt(x.state - 1)))

  implicit object addNatAsInt extends Implementation[nat.add.type, nat.sort ->: nat.sort ->: nat.sort, NATasInt => NATasInt => NATasInt] {
    override def apply(): NATasInt => NATasInt => NATasInt = x => y => NATasInt(x.state + y.state)
  }

}


