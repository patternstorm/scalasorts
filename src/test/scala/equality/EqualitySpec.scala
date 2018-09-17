package equality

import org.scalatest.{FunSpec, GivenWhenThen, Matchers}
import universe.Universe._

class EqualitySpec extends FunSpec with Matchers with GivenWhenThen with Equality {

  describe("equality") {
    it ("is reflexive") {
      type aThing = aThing.type
      object aThing extends Operator
      type anotherThing = anotherThing.type
      object anotherThing extends Operator
      type aKind = aKind.type
      object aKind extends Sort
      implicitly[Equals[aThing, aThing]]
      "implicitly[Equals[aThing, anotherThing]]" shouldNot compile
      "implicitly[Equals[anotherThing, aKind]]" shouldNot compile
      "implicitly[Equals[aKind, aThing]]" shouldNot compile
    }
    it ("is transitive") {
      type a = a.type
      object a extends Operator
      type b = b.type
      object b extends Operator
      type c = c.type
      object c extends Sort
      //
      implicit val eq1: Equals[a,b] = new Equals[a,b] {}
      implicit val eq2: Equals[b,c] = new Equals[b,c] {}
      implicitly[Equals[a,c]]
    }
  }
}
