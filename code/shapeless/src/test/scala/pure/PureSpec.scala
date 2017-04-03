package pure

import org.scalatest._

case class Foo(a: String, b: Int, c: Boolean)

class PureSpec extends FreeSpec with Matchers {
  "pure type class with shapeless" in {
    val actual   = Pure[Foo].value
    val expected = Foo("", 0, false)
    actual should be(expected)
  }
}
