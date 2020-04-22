package com.ysthakur.javamm.parsing.ast.infile

import com.ysthakur.javamm.parsing.ast.Types._

import scala.collection.mutable.ListBuffer

class Field(override val name: String,
                 typeRef: Option[TypeRef] = None,
                 var varType: Some[TypeRef],
                 mods: Iterable[Modifier],
                 var initExpr: Option[Expr] = None) extends IVariableDecl with HasModifiers {
  override val modifiers: ModifierList = ModifierList(ListBuffer().addAll(mods))
  override def text: String = ???
}
