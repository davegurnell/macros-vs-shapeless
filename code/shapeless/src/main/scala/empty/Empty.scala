package empty

import shapeless._

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

  implicit val hnilEmpty: Empty[HNil] =
    pure(HNil)

  implicit def hconsEmpty[H, T <: HList](
    implicit
    hEmpty: Lazy[Empty[H]],
    tEmpty: Empty[T]
  ): Empty[H :: T] =
    pure(hEmpty.value.create :: tEmpty.create)

  implicit def genericEmpty[A, R](
    implicit
    gen: Generic.Aux[A, R],
    empty: Lazy[Empty[R]]
  ): Empty[A] =
    pure(gen.from(empty.value.create))
}
