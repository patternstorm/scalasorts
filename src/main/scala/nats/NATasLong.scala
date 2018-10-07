package nats

import universe.Universe._

case class NATasLong(val value: Long) extends AnyVal

object NATasLong extends nat.imp[NATasLong] {
  implicit def asLong(x: NATasLong): Long = x.value

  implicit def asNatAsLong(x: Long): NATasLong = NATasLong(x)

  override def _encode(x: nat.zero.rep): NATasLong = 0

  override def _encode(x: nat.succ.rep): NATasLong = asImp(x._1) + 1

  override def _decode(x: NATasLong): nat.rep = if (x.value == 0) nat.zero.rep else nat.succ.rep(_decode(x - 1))

  implicit object addNatAsLong extends Implementation[nat.add.type, nat.sort ->: nat.sort ->: nat.sort, NATasLong => NATasLong => NATasLong] {
    override def apply(): NATasLong => NATasLong => NATasLong = x => y => x + y
  }

}
