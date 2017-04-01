package empty

import org.scalatest._

case class Foo(a: String, b: Int, c: Boolean)

class EmptySpec extends FreeSpec with Matchers {
  "empty type class with shapeless" in {
    val actual   = Empty[Foo].create
    val expected = Foo("", 0, false)
    actual should be(expected)
  }
}
