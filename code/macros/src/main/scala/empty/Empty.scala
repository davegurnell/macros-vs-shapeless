package empty

import scala.language.experimental.macros

trait Empty[A] {
  def create: A
}

object Empty {
  def pure[A](a: A): Empty[A] =
    new Empty[A] { def create = a }

  def apply[A](implicit empty: Empty[A]): Empty[A] =
    empty

  implicit val stringEmpty: Empty[String] =
    pure("")

  implicit val intEmpty: Empty[Int] =
    pure(0)

  implicit val booleanEmpty: Empty[Boolean] =
    pure(false)

  implicit def generic[A]: Empty[A] =
    macro EmptyMacros.genericImpl[A]
}
