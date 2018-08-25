package bools


import org.scalatest.{FunSpec, GivenWhenThen, Matchers}
import universe.Universe


class BoolsSpec extends FunSpec with Matchers with GivenWhenThen
with Universe with Bools with BoolsWithBooleans {

  describe("..") {
    it("...") {
      implicitly[nothing]
      implicitly[BOOL.sort]

      implicitly[BOOL.True]
      implicitly[Morphism[BOOL.True, BOOL.sort]]

      implicitly[BOOL.False]
      implicitly[Morphism[BOOL.False, BOOL.sort]]

      implicitly[BOOL.not]
      implicitly[Morphism[BOOL.not, BOOL.sort ->: BOOL.sort]]

      implicitly[BOOL.and]
      implicitly[Morphism[BOOL.and, BOOL.sort ->: BOOL.sort ->: BOOL.sort]]

      implicitly[BOOL.or]
      implicitly[Morphism[BOOL.or, BOOL.sort ->: BOOL.sort ->: BOOL.sort]]
      
      implicitly[Morphism[BOOL.and ∙ (BOOL.not ∙ BOOL.True), BOOL.sort ->: BOOL.sort]]
      
      val z = implicitly[Morphism[(BOOL.or ∙ (BOOL.not ∙ BOOL.False)) ∙ BOOL.True, BOOL.sort]]
      z().state shouldEqual true
      val y = implicitly[Morphism[BOOL.not ∙ ((BOOL.and ∙ (BOOL.not ∙ BOOL.True)) ∙ BOOL.True), BOOL.sort]]
      y().state shouldEqual true
      val x = implicitly[Morphism[(BOOL.and ∙ (BOOL.not ∙ BOOL.False)) ∙ (BOOL.not ∙ ((BOOL.or ∙ (BOOL.not ∙ BOOL.True)) ∙ BOOL.False)), BOOL.sort]]
      val rep: BOOL.sort#rep = x()
      rep.state shouldEqual true
      //implicitly[(BOOL.not ∙ BOOL.not) ->: BOOL.nat]

      val term1 : BOOL.not ∙ BOOL.True = implicitly[BOOL.not ∙ BOOL.True]
      val term2 : (BOOL.and ∙ (BOOL.not ∙ BOOL.False)) ∙ (BOOL.not ∙ ((BOOL.or ∙ (BOOL.not ∙ BOOL.True)) ∙ BOOL.False)) = implicitly[(BOOL.and ∙ (BOOL.not ∙ BOOL.False)) ∙ (BOOL.not ∙ ((BOOL.or ∙ (BOOL.not ∙ BOOL.True)) ∙ BOOL.False))]
      //val term2 = implicitly[BOOL.not ∙ BOOL.not]


      val n  = BOOL.and(BOOL.not(BOOL.False()),BOOL.not(BOOL.True()))
      val w: Boolean = n()
      w shouldEqual false

      val l  = BOOL.or(BOOL.not(BOOL.True()),BOOL.not(BOOL.False()))
      val v: Boolean = l()
      v shouldEqual true

      import BOOL._
      val m = and(BOOL.not(True()),BOOL.not(False()))
      m().state shouldEqual false

      val s =implicitly[Morphism[BOOL.not, BOOL.sort ->: BOOL.sort]]
      s()(n()).state shouldEqual true
      val k = BOOL.not(n)
      k().state shouldEqual true

      BOOL.and

      BOOL.and.hashCode() shouldNot equal (BOOL.not.hashCode())

    }
  }
}
