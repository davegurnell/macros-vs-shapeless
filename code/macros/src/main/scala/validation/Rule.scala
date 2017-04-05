package validation

import scala.language.experimental.macros

trait Rule[A] {
  def apply(value: A): List[Error]

  def and(that: Rule[A]): Rule[A] =
    value => this(value) ++ that(value)

  def contramap[B](func: B => A): Rule[B] =
    value => this(func(value))

  def prefixed(field: String): Rule[A] =
    value => this(value).map(_.prefixed(field))

  def at[B](func: B => A): Rule[B] =
    macro Macros.atMacro

  def field[B](func: A => B)(rule: Rule[B]): Rule[A] =
    macro Macros.fieldMacro
}

object Rule {
  def apply[A]: Rule[A] =
    value => Nil

  def gte(target: Int): Rule[Int] =
    value => if(value >= target) Nil else List(Error(s"Must be >= $target"))

  def nonEmpty: Rule[String] =
    value => if(value.nonEmpty) Nil else List(Error(s"Must be non-empty"))
}
