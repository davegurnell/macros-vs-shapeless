import scala.language.experimental.macros

package object create {
  def create[A]: A =
    macro Macros.createMacro[A]
}
