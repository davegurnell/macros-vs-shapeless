package pure

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

class Macros(val c: blackbox.Context) {
  import c.universe._

  def genericPureMacro[A: WeakTypeTag]: Tree = {
    val targetType = weakTypeOf[A]

    val applyMethod = findApplyMethod(targetType)

    val applyParams = applyMethod.paramLists.map { paramList =>
      paramList.map { param =>
        createApplyParam(param.typeSignature)
      }
    }

    q"""
    new _root_.pure.Pure[$targetType] {
      def value = $applyMethod(...$applyParams)
    }
    """
  }

  def findApplyMethod(targetType: Type): MethodSymbol =
    findCompanion(targetType)
      .members
      .find(isApplyMethod(targetType))
      .map(_.asMethod)
      .getOrElse(c.abort(c.enclosingPosition, "Could not find apply method"))

  def findCompanion(targetType: Type): Type =
    if(targetType.companion == NoType) {
      c.abort(c.enclosingPosition, "Could not find companion object")
    } else {
      targetType.companion
    }

  // This one stolen from Macwire:
  def isApplyMethod(tergetType: Type)(member: Symbol): Boolean =
    member.isMethod &&
    member.isPublic &&
    member.asMethod.returnType <:< tergetType &&
    member.asMethod.name.decodedName.toString == "apply"

  def createApplyParam(paramType: Type): Tree =
    q"_root_.pure.Pure[$paramType].value"
}
