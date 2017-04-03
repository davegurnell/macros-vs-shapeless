package validation

import scala.annotation.implicitNotFound
import shapeless._
import shapeless.ops.record.Selector

/** Evidence that a container of type A has a field called K of type B. */
@implicitNotFound("Could not prove that ${A} has a field called ${K} of type ${B}")
trait HasField[A, K, B] {
  def name: String
  def zoom(value: A): B

  def apply(rule: Rule[B]): Rule[A] =
    rule.prefixed(name).contramap(zoom)
}

object HasField {

  implicit def recordHasField[L <: HList, K, F](
    implicit
    ev: K <:< Symbol,
    witness: Witness.Aux[K],
    selector: Selector.Aux[L, K, F]
  ): HasField[L, K, F] =
    new HasField[L, K, F] {
      val name = witness.value.name
      def zoom(value: L): F = selector(value)
    }

  implicit def genericHasField[A, L, K, F](
    implicit
    ev: K <:< Symbol,
    generic: LabelledGeneric.Aux[A, L],
    hasField: HasField[L, K, F]
  ): HasField[A, K, F] =
    new HasField[A, K, F] {
      val name = hasField.name
      def zoom(value: A): F = hasField.zoom(generic.to(value))
    }

}
