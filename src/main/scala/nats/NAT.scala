package nats

import dsl.Statements._
import universe.Universe._

object NAT {

  @sort trait nat {
    var x, y: nat

    def zero: () => nat

    def succ: nat => nat

    def add: (nat, nat) => nat = {
      (zero(), x) -> x
      (succ(x), y) -> succ(add(x, y))
    }
  }

}