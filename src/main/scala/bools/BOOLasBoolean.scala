package bools

case class BOOLasBoolean(state: Boolean)

object BOOLasBoolean extends bool.imp[BOOLasBoolean] {

  override protected def _encode(x: bool.`true`.rep): BOOLasBoolean = BOOLasBoolean(true)

  override protected def _encode(x: bool.`false`.rep): BOOLasBoolean = BOOLasBoolean(false)

  override protected def _decode(x: BOOLasBoolean): bool.rep = if (x.state) bool.`true`.rep else bool.`false`.rep

}

