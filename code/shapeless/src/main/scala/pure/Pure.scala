package pure

import shapeless._

trait Pure[A] {
  def value: A
}

object Pure {
  def instance[A](a: A): Pure[A] =
    new Pure[A] { def value = a }

  def apply[A](implicit pure: Pure[A]): Pure[A] =
    pure

  implicit val stringPure: Pure[String] =
    instance("")

  implicit val intPure: Pure[Int] =
    instance(0)

  implicit val booleanPure: Pure[Boolean] =
    instance(false)

  implicit val hnilPure: Pure[HNil] =
    instance(HNil)

  implicit def hconsPure[H, T <: HList](
    implicit
    hPure: Lazy[Pure[H]],
    tPure: Pure[T]
  ): Pure[H :: T] =
    instance(hPure.value.value :: tPure.value)

  implicit def genericPure[A, R](
    implicit
    gen: Generic.Aux[A, R],
    pure: Lazy[Pure[R]]
  ): Pure[A] =
    instance(gen.from(pure.value.value))
}
