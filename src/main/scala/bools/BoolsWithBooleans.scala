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
        override type Rep = BOOLasBoolean
      }
      override val sort : sort = bool
      override private[bools] def trueImp(x: Nothing#Rep): BOOLasBoolean = BOOLasBoolean(true)
      override private[bools] def falseImp(x: Nothing#Rep): BOOLasBoolean = BOOLasBoolean(false)
      override private[bools] def notImp(x: BOOLasBoolean): BOOLasBoolean = BOOLasBoolean(!x.state)
      override private[bools] def andImp(x: BOOLasBoolean, y: BOOLasBoolean): BOOLasBoolean = BOOLasBoolean(x.state && y.state)
      override private[bools] def orImp(x: BOOLasBoolean, y: BOOLasBoolean): BOOLasBoolean = BOOLasBoolean(x.state || y.state)
    }

}
