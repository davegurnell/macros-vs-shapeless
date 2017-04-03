package pure

import scala.language.experimental.macros

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

  implicit def genericPure[A]: Pure[A] =
    macro Macros.genericPureMacro[A]
}
