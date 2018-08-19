package universe

import java.util.UUID

trait Individuals {
    sealed trait Individual //{val identity: UUID = UUID.randomUUID()}
    trait Universal extends Individual { type Rep }
    trait Particular extends Individual
}
