package validation

import org.scalatest._

case class IceCream(name: String, cherries: Int, cone: Boolean)

class RuleSpec extends FreeSpec with Matchers {

  "gte" in {
    val rule = Rule.gte(0)

    rule(1) should be(Nil)
    rule(0) should be(Nil)
    rule(-1) should be(List(Error("Must be >= 0")))
  }

  "nonEmpty" in {
    val rule = Rule.nonEmpty

    rule("foo") should be(Nil)
    rule(" ") should be(Nil)
    rule("") should be(List(Error("Must be non-empty")))
  }

  "contramap" in {
    val rule1 = Rule.gte(0).contramap[(Int, Int)](_._1)
    val rule2 = Rule.gte(0).contramap[(Int, Int)](_._2)

    rule1((0, 0)) should be(Nil)
    rule2((0, 0)) should be(Nil)
    rule1((-1, 0)) should be(List(Error("Must be >= 0")))
    rule2((0, -1)) should be(List(Error("Must be >= 0")))
  }

  "prefixed" in {
    val rule1 = Rule.gte(0).prefixed("foo")
    val rule2 = Rule.gte(0).prefixed("foo").prefixed("bar")

    rule1(0) should be(Nil)
    rule1(-1) should be(List(Error("Must be >= 0", "foo" :: Nil)))
    rule2(-1) should be(List(Error("Must be >= 0", "bar" :: "foo" :: Nil)))
  }

  "at" in {
    val rule = Rule.gte(0).at[IceCream](_.cherries)

    rule(IceCream("Sundae", 1, false)) should be(Nil)
    rule(IceCream("Sundae", -1, false)) should be(List(Error("Must be >= 0", "cherries" :: Nil)))
  }

  "field" in {
    import Rule._

    val rule = Rule[IceCream]
      .field(_.name)(nonEmpty)
      .field(_.cherries)(gte(0))

    rule(IceCream("Sundae", 1, false)) should be(Nil)

    rule(IceCream("Sundae", -1, false)) should be(List(
      Error("Must be >= 0", "cherries" :: Nil)
    ))

    rule(IceCream("", -1, false)) should be(List(
      Error("Must be non-empty", "name" :: Nil),
      Error("Must be >= 0", "cherries" :: Nil)
    ))
  }

}
