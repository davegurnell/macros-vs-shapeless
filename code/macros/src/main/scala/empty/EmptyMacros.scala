package empty

import scala.language.experimental.macros
import scala.language.higherKinds
import scala.reflect.macros.blackbox.Context

class EmptyMacros(val c: Context) {
  import c.universe._

  val stringType: Type  = typeOf[String]
  val intType: Type     = typeOf[Int]
  val booleanType: Type = typeOf[Boolean]

  def genericImpl[A: c.WeakTypeTag]: c.Tree = {
    val tpe: Type =
      weakTypeOf[A]

    val method: MethodSymbol =
      applyMethod(tpe)

    val args: List[List[Tree]] =
      method.paramLists.map { paramList =>
        paramList.map { param =>
          parameter(param.name, param.typeSignature)
        }
      }

    q"""
    new _root_.empty.Empty[$tpe] {
      def create = $method(...$args)
    }
    """
  }

  // This one stolen from Macwire:
  def isCompanionApply(tpe: Type)(method: Symbol): Boolean =
    method.isMethod &&
    method.isPublic &&
    method.asMethod.returnType <:< tpe &&
    method.asMethod.name.decodedName.toString == "apply"

  def companionType(tpe: Type): Type =
    if(tpe.companion == NoType) {
      c.abort(c.enclosingPosition, "Could not find companion")
    } else {
      tpe.companion
    }

  def applyMethod(tpe: Type): MethodSymbol =
    companionType(tpe)
      .members
      .find(isCompanionApply(tpe))
      .map(_.asMethod)
      .getOrElse(c.abort(c.enclosingPosition, "Could not find apply method"))

  def parameter(name: Name, tpe: Type): Tree =
    q"""_root_.empty.Empty[$tpe].create"""
}
