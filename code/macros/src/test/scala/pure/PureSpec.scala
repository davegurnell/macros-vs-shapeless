package pure

import org.scalatest._

case class IceCream(name: String, cherries: Int, cone: Boolean)

class PureSpec extends FreeSpec with Matchers {
  "pure type class with macros" in {
    val actual   = Pure[IceCream].value
    val expected = IceCream("", 0, false)
    actual should be(expected)
  }
}
