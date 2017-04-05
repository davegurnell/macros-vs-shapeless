package validation

import shapeless.Witness

trait Rule[A] {
  def apply(value: A): List[Error]

  def and(that: Rule[A]): Rule[A] =
    value => this(value) ++ that(value)

  def contramap[B](func: B => A): Rule[B] =
    value => this(func(value))

  def prefixed(field: String): Rule[A] =
    value => this(value).map(_.prefixed(field))

  def at[B](k: Witness)(implicit ev: HasField[B, k.T, A]): Rule[B] =
    ev(this)

  def field[B](k: Witness)(rule: Rule[B])(implicit ev: HasField[A, k.T, B]): Rule[A] =
    this and ev(rule)
}

object Rule {
  def apply[A]: Rule[A] =
    value => Nil

  def gte(target: Int): Rule[Int] =
    value => if(value >= target) Nil else List(Error(s"Must be >= $target"))

  def nonEmpty: Rule[String] =
    value => if(value.nonEmpty) Nil else List(Error(s"Must be non-empty"))
}
