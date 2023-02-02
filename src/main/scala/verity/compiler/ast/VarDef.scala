package verity.compiler.ast

/** A variable definition (parameter, field, local variable, or global variable) */
sealed trait VarDef {
  def name: String
  def typ: Type
}

case class Param(name: String, typ: Type) extends VarDef

case class GlobalVar(name: String, typ: Type, expr: Expr) extends VarDef

case class LocalVar(name: String, typ: Type, expr: Expr) extends VarDef
