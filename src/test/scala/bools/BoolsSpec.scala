package bools


import org.scalatest.{FunSpec, GivenWhenThen, Matchers}
import universe.Universe._


class BoolsSpec extends FunSpec with Matchers with GivenWhenThen
  with Bools with BoolWithBooleans {

  describe("..") {
    it("...") {
      implicitly[nothing]
      implicitly[bool]

      implicitly[bool.`true`]
      implicitly[Morphism[bool.`true`, bool]]

      implicitly[bool.`false`]
      implicitly[Morphism[bool.`false`, bool]]

      implicitly[bool.not]
      implicitly[Morphism[bool.not, bool ->: bool]]

      implicitly[bool.and]
      implicitly[Morphism[bool.and, bool ->: bool ->: bool]]

      implicitly[bool.or]
      implicitly[Morphism[bool.or, bool ->: bool ->: bool]]

      implicitly[Morphism[bool.and ∙ (bool.not ∙ bool.`true`), bool ->: bool]]

      //val z = implicitly[Morphism[(bool.or ∙ (bool.not ∙ bool.`false`)) ∙ bool.`true`, bool]]
      val z = implicitly[Morphism[bool.not ∙ bool.`false`, bool]]
      var value: BOOLasBoolean = z()
      value.state shouldEqual true
      val y = implicitly[Morphism[bool.not ∙ ((bool.and ∙ (bool.not ∙ bool.`true`)) ∙ bool.`true`), bool]]
      value = y()
      value.state shouldEqual true
      val x = implicitly[Morphism[(bool.and ∙ (bool.not ∙ bool.`false`)) ∙ (bool.not ∙ ((bool.or ∙ (bool.not ∙ bool.`true`)) ∙ bool.`false`)), bool]]
      value = x()
      value.state shouldEqual true
      //implicitly[(bool.not ∙ bool.not) ->: bool.nat]

      val term1: bool.not ∙ bool.`true` = implicitly[bool.not ∙ bool.`true`]
      val term2: (bool.and ∙ (bool.not ∙ bool.`false`)) ∙ (bool.not ∙ ((bool.or ∙ (bool.not ∙ bool.`true`)) ∙ bool.`false`)) = implicitly[(bool.and ∙ (bool.not ∙ bool.`false`)) ∙ (bool.not ∙ ((bool.or ∙ (bool.not ∙ bool.`true`)) ∙ bool.`false`))]
      //val term2 = implicitly[bool.not ∙ bool.not]


      val n = bool.and(bool.not(bool.`false`()), bool.not(bool.`true`()))
      value = n()
      value.state shouldEqual false

      val l = bool.or(bool.not(bool.`true`()), bool.not(bool.`false`()))
      value = l()
      value.state shouldEqual true

      import bool._
      val m = and(bool.not(`true`()), bool.not(`false`()))
      value = m()
      value.state shouldEqual false

      val s = implicitly[Morphism[bool.not, bool ->: bool]]
      value = s()(n())
      value.state shouldEqual true
      val k = bool.not(n)
      value = k()
      value.state shouldEqual true

      bool.and

      bool.and.hashCode() shouldNot equal(bool.not.hashCode())

    }
  }
}
