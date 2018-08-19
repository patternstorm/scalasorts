package equality

import org.scalatest.{FunSpec, GivenWhenThen, Matchers}
import universe.Universe

class EqualitySpec extends FunSpec with Matchers with GivenWhenThen with Universe with Equality {

  describe("equality") {
    it ("is reflexive") {
      type aThing = aThing.type
      object aThing extends Particular
      type anotherThing = anotherThing.type
      object anotherThing extends Particular
      type aKind = aKind.type
      object aKind extends Universal
      implicitly[Equals[aThing, aThing]]
      "implicitly[Equals[aThing, anotherThing]]" shouldNot compile
      "implicitly[Equals[anotherThing, aKind]]" shouldNot compile
      "implicitly[Equals[aKind, aThing]]" shouldNot compile
    }
    it ("is transitive") {
      type a = a.type
      object a extends Particular
      type b = b.type
      object b extends Particular
      type c = c.type
      object c extends Universal
      //
      implicit val eq1: Equals[a,b] = new Equals[a,b] {}
      implicit val eq2: Equals[b,c] = new Equals[b,c] {}
      implicitly[Equals[a,c]]
    }
  }
}
