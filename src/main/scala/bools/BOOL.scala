package bools

import dsl.Statements._
import universe.Universe._


object BOOL {

  @sort trait bool {
    var x, y: bool

    def `true`: () => bool

    def `false`: () => bool

    def not: bool => bool = {
      `true`() -> `false`()
      `false`() -> `true`()
    }

    def and: (bool, bool) => bool = {
      (`true`(), x) -> x
      (`false`(), x) -> `false`()
    }

    def or: (bool, bool) => bool = {
      (`true`(), x) -> `true`()
      (`false`(), x) -> x
    }
  }

}
