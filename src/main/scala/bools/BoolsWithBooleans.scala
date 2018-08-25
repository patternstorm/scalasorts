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

      implicit object trueImp extends (True :: bool) {
        override def apply(x: nothing#rep): BOOLasBoolean = BOOLasBoolean(true)
      }

      implicit object falseImp extends (False :: bool) {
        override def apply(x: nothing#rep): BOOLasBoolean = BOOLasBoolean(false)
      }

      implicit object notImp extends (not :: bool ->: bool) {
        private def notImp(x: BOOLasBoolean): BOOLasBoolean = BOOLasBoolean(!x.state)
        override def apply(n: nothing#rep): BOOLasBoolean => BOOLasBoolean = x => notImp(x)
      }

      implicit object andImp extends (and :: bool ->: bool ->: bool) {
        override def apply(n: nothing#rep): BOOLasBoolean => BOOLasBoolean => BOOLasBoolean = x => y => andImp(x,y)
        private def andImp(x: BOOLasBoolean, y: BOOLasBoolean): BOOLasBoolean = BOOLasBoolean(x.state && y.state)
      }

      implicit object orImp extends (or :: bool ->: bool ->: bool) {
        override def apply(n: nothing#rep): BOOLasBoolean => BOOLasBoolean => BOOLasBoolean = x => y => orImp(x,y)
        private def orImp(x: BOOLasBoolean, y: BOOLasBoolean): BOOLasBoolean = BOOLasBoolean(x.state || y.state)
      }
    }

}
