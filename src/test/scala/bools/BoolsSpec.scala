package bools


import org.scalatest.{FunSpec, GivenWhenThen, Matchers}
import universe.Universe._
import bools.BOOL._


class BoolsSpec extends FunSpec with Matchers with GivenWhenThen {

  describe("..") {
    it("...") {
      type BOOLasBoolean = BOOLasBoolean.type
      type bool = bool.sort

      val bool1: Rep[BOOLasBoolean] = bool.not(bool.`false`())
      bool1.state shouldEqual true


      val bool2: Rep[BOOLasBoolean] = bool.and(bool.not(bool.`false`()), bool.not(bool.`true`()))
      bool2.state shouldEqual false

      val bool3: Rep[BOOLasBoolean] = bool.or(bool.not(bool.`true`()), bool.not(bool.`false`()))
      bool3.state shouldEqual true

      import bools.BOOL.bool._
      val bool4: Rep[BOOLasBoolean] = and(bool.not(`true`()), bool.not(`false`()))
      bool4.state shouldEqual false

      val bool5: Rep[BOOLasBoolean] = bool.not(bool4)
      bool5.state shouldEqual true

      bool.and

      bool.and.hashCode() shouldNot equal(bool.not.hashCode())

    }
  }
}
