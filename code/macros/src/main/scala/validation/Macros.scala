package validation

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

class Macros(val c: blackbox.Context) {
  import c.universe._

  def atMacro(func: Tree): Tree = {
    val name = q"""${accessorName(func).toString}"""
    q"${c.prefix}.prefixed($name).contramap($func)"
  }

  def fieldMacro(func: Tree)(rule: Tree): Tree =
    q"${c.prefix} and $rule.at($func)"

  def accessorName(accessor: Tree): TermName =
    accessor match {
      case q"($param) => $obj.$name" => name
      case other => c.abort(c.enclosingPosition, "Argument must be an accessor method.")
    }
}
