package create

import scala.language.experimental.macros
import scala.language.higherKinds
import scala.reflect.macros.blackbox

class Macros(val c: blackbox.Context) {
  import c.universe._

  /**
   * Expand an expression of form:
   *
   * {{create[IceCream]}}
   *
   * into a constructor expression of form:
   *
   * {{IceCream("", 0, false)}}
   *
   * Attempt to infer the correct value
   * for each parameter.
   */
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

  /**
   * Find the first method called `apply`
   * in the companion object of {{targetType}}.
   *
   * Fail if it doesn't have a companion object.
   */
  def findApplyMethod(targetType: Type): MethodSymbol =
    findCompanion(targetType)
      .members
      .find(isApplyMethod(targetType))
      .map(_.asMethod)
      .getOrElse(c.abort(c.enclosingPosition, "Could not find apply method"))

  /**
   * Find the companion object of {{targetType}}.
   *
   * Fail if it doesn't have a companion object.
   */
  def findCompanion(targetType: Type): Type =
    if(targetType.companion == NoType) {
      c.abort(c.enclosingPosition, "Could not find companion object")
    } else {
      targetType.companion
    }

  /**
   * Check whether {{member}} is an `apply` method
   * returning an instance of {{targetType}}.
   */
  def isApplyMethod(targetType: Type)(member: Symbol): Boolean =
    member.isMethod &&
    member.isPublic &&
    member.asMethod.returnType <:< targetType &&
    member.asMethod.name.decodedName.toString == "apply"

  /**
   * Create an appropriate parameter
   * of type {{paramType}} to pass to `apply`.
   *
   * This implementation only handles three types:
   * String, Int, and Boolean.
   */
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








