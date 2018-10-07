package equality

import org.scalatest.{FunSpec, GivenWhenThen, Matchers}
import universe.Universe._

class EqualitySpec extends FunSpec with Matchers with GivenWhenThen with Equality {

  describe("equality") {
    it ("is reflexive") {
      type aThing = aThing.type
      object aThing extends Operator {
        override val symbol = "aThing"
      }
      type anotherThing = anotherThing.type
      object anotherThing extends Operator {
        override val symbol = "anotherThing"
      }
      type aKind = aKind.type
      object aKind extends Sort {
        override val symbol = "aKind"
      }
      implicitly[Equals[aThing, aThing]]
      "implicitly[Equals[aThing, anotherThing]]" shouldNot compile
      "implicitly[Equals[anotherThing, aKind]]" shouldNot compile
      "implicitly[Equals[aKind, aThing]]" shouldNot compile
    }
    it ("is transitive") {
      type a = a.type
      object a extends Operator {
        override val symbol = "a"
      }
      type b = b.type
      object b extends Operator {
        override val symbol = "b"
      }
      type c = c.type
      object c extends Sort {
        override val symbol = "c"
      }
      //
      implicit val eq1: Equals[a,b] = new Equals[a,b] {}
      implicit val eq2: Equals[b,c] = new Equals[b,c] {}
      implicitly[Equals[a,c]]
    }
  }
}
