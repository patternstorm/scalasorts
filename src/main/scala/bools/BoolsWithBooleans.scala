package bools

import universe.Universe
import scala.language.implicitConversions

trait BoolsWithBooleans {
    self: Universe with Bools =>

    case class BOOLasBoolean(state: Boolean)
    object BOOLasBoolean { implicit def asBoolean(x: BOOLasBoolean): Boolean = x.state }

    object BOOL extends BOOL {
      override type sort = bool.type
      implicit object bool extends Universal {
        override type rep = BOOLasBoolean
      }
      override val sort : sort = bool

      implicit object trueImp extends (True :: nothing ->: bool) {
        override def apply(x: nothing#rep): BOOLasBoolean = BOOLasBoolean(true)
      }

      implicit object falseImp extends (False :: nothing ->: bool) {
        override def apply(x: nothing#rep): BOOLasBoolean = BOOLasBoolean(false)
      }

      implicit object notImp extends (not :: bool ->: bool) {
        override def apply(x: BOOLasBoolean): BOOLasBoolean = BOOLasBoolean(!x.state)
      }

      implicit object andImp extends (and :: bool ->: bool ->: bool) {
        override def apply(x: BOOLasBoolean): BOOLasBoolean => BOOLasBoolean = andImp(x,_)
        private def andImp(x: BOOLasBoolean, y: BOOLasBoolean): BOOLasBoolean = BOOLasBoolean(x.state && y.state)
      }

      implicit object orImp extends (or :: bool ->: bool ->: bool) {
        override def apply(x: BOOLasBoolean): BOOLasBoolean => BOOLasBoolean   = orImp(x,_)
        private def orImp(x: BOOLasBoolean, y: BOOLasBoolean): BOOLasBoolean = BOOLasBoolean(x.state || y.state)
      }
    }

}
