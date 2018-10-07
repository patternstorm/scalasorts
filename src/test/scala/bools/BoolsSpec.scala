package bools


import org.scalatest.{FunSpec, GivenWhenThen, Matchers}
import universe.Universe._


class BoolsSpec extends FunSpec with Matchers with GivenWhenThen {

  describe("..") {
    it("...") {
      type bool = bool.sort
      implicitly[bool as BOOLasBoolean]
      var value: BOOLasBoolean = bool.`true`.rep
      val rep: bool.rep = value
      //      implicitly[nothing]
      //      implicitly[bool]
      //
      //      implicitly[bool.`true`.type]
      //      implicitly[Morphism[bool.`true`.type, bool]]
      //
      //      implicitly[bool.`false`.type]
      //      implicitly[Morphism[bool.`false`.type, bool]]
      //
      //      implicitly[bool.not.type]
      //      implicitly[Morphism[bool.not.type, bool ->: bool]]
      //
      //      implicitly[bool.and.type]
      //      implicitly[Morphism[bool.and.type, bool ->: bool ->: bool]]
      //
      //      implicitly[bool.or.type]
      //      implicitly[Morphism[bool.or.type, bool ->: bool ->: bool]]
      //
      //      implicitly[Morphism[bool.and.type ∙ (bool.not.type ∙ bool.`true`.type), bool ->: bool]]
      //
      //      val z = implicitly[Morphism[(bool.or.type ∙ (bool.not.type ∙ bool.`false`.type)) ∙ bool.`true`.type, bool]]
      //      var value: BOOLasBoolean = z()
      //      value.state shouldEqual true
      //      val y = implicitly[Morphism[bool.not ∙ ((bool.and ∙ (bool.not ∙ bool.`true`)) ∙ bool.`true`), bool]]
      //      value = y()
      //      value.state shouldEqual true
      //      val x = implicitly[Morphism[(bool.and ∙ (bool.not ∙ bool.`false`)) ∙ (bool.not ∙ ((bool.or ∙ (bool.not ∙ bool.`true`)) ∙ bool.`false`)), bool]]
      //      value = x()
      //      value.state shouldEqual true
      //      //implicitly[(bool.not ∙ bool.not) ->: bool.nat]
      //
      //      val term1: bool.not ∙ bool.`true` = implicitly[bool.not ∙ bool.`true`]
      //      val term2: (bool.and ∙ (bool.not ∙ bool.`false`)) ∙ (bool.not ∙ ((bool.or ∙ (bool.not ∙ bool.`true`)) ∙ bool.`false`)) = implicitly[(bool.and ∙ (bool.not ∙ bool.`false`)) ∙ (bool.not ∙ ((bool.or ∙ (bool.not ∙ bool.`true`)) ∙ bool.`false`))]
      //val term2 = implicitly[bool.not ∙ bool.not]

      val T = bool.`true`()
      val trueRep: bool.rep = T()
      trueRep shouldEqual bool.`true`.rep


      val n = bool.and(bool.not(bool.`false`()), bool.not(bool.`true`()))
      value = BOOLasBoolean.asImp(n())
      value.state shouldEqual false

      val l = bool.or(bool.not(bool.`true`()), bool.not(bool.`false`()))
      value = l()
      value.state shouldEqual true
      val m = and(bool.not(`true`()), bool.not(`false`()))
      value = m()
      value.state shouldEqual false

      val s = implicitly[Morphism[bool.not.type, bool ->: bool]]
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
