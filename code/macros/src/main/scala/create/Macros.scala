package create

import scala.language.experimental.macros
import scala.language.higherKinds
import scala.reflect.macros.blackbox

class Macros(val c: blackbox.Context) {
  import c.universe._

  def createMacro[A: WeakTypeTag]: Tree = {
    val targetType = weakTypeOf[A]

    val applyMethod = findApplyMethod(targetType)

    val applyParams = applyMethod.paramLists.map { paramList =>
      paramList.map { param =>
        createApplyParam(param.typeSignature)
      }
    }

    q"$applyMethod(...$applyParams)"
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

  def isApplyMethod(targetType: Type)(member: Symbol): Boolean =
    member.isMethod &&
    member.isPublic &&
    member.asMethod.returnType <:< targetType &&
    member.asMethod.name.decodedName.toString == "apply"

  def createApplyParam(paramType: Type): Tree =
    if(paramType <:< typeOf[String]) {
      q""" "" """
    } else if(paramType <:< typeOf[Int]) {
      q"0"
    } else if(paramType <:< typeOf[Boolean]) {
      q"false"
    } else {
      c.abort(c.enclosingPosition, "Could not fill in parameter for type: " + show(paramType))
    }
}
