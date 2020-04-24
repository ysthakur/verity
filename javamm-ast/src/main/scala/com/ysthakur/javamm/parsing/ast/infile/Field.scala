package com.ysthakur.javamm.parsing.ast.infile

import com.ysthakur.javamm.parsing.ast.infile.expr.Expr

import scala.collection.mutable.ListBuffer

class Field(
               override val name: String,
               typeRef: Option[TypeRef] = None,
               private var _myType: ITypeRef | Null,
               override val modifiers: ModifierList,
               var initExpr: Option[Expr] = None
           ) extends IVariableDecl
    with HasModifiers
    with HasType {
  override def text: String = ???
  override def myType: Option[ITypeRef] = Option(_myType)
  override def myType_=(varType: ITypeRef): Unit = _myType = varType
}