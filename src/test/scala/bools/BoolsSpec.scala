package bools


import org.scalatest.{FunSpec, GivenWhenThen, Matchers}
import universe.Universe._
import bools.BOOL._


class BoolsSpec extends FunSpec with Matchers with GivenWhenThen {

  describe("..") {
    it("...") {
      type bool = bool.sort
      implicitly[bool as BOOLasBoolean]
      var value: BOOLasBoolean = bool.`true`.rep
      var rep: bool.rep = value
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


      //val T = bool.`true`()
      //      rep = bool.`true`()
      //      rep shouldEqual bool.`true`.rep
      //      value = bool.`true`()
      //      value shouldEqual BOOLasBoolean(true)
      //      rep = value

      val t0 = bool.not(bool.`false`())
      t0.as[BOOLasBoolean].state shouldEqual true


      val t1 = bool.and(bool.not(bool.`false`()), bool.not(bool.`true`()))
      value = BOOLasBoolean.asImp(t1.as[BOOLasBoolean])
      value.state shouldEqual false

      val t2 = bool.or(bool.not(bool.`true`()), bool.not(bool.`false`()))
      value = t2.as[BOOLasBoolean]
      value.state shouldEqual true
      import bools.BOOL.bool._
      val t3 = and(bool.not(`true`()), bool.not(`false`()))
      value = t3.as[BOOLasBoolean]
      value.state shouldEqual false

      val s = implicitly[Morphism[bool.not.type, bool ->: bool]]
      value = s()(t3.as[BOOLasBoolean])
      value.state shouldEqual true
      val t4 = bool.not(t3)
      value = t4.as[BOOLasBoolean]
      value.state shouldEqual true

      bool.and

      bool.and.hashCode() shouldNot equal(bool.not.hashCode())

    }
  }
}
