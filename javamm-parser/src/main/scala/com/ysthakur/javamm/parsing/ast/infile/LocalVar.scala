package com.ysthakur.javamm.parsing.ast.infile

import com.ysthakur.javamm.parsing.ast.infile.expr.Expr

class LocalVar(override val name: String,
               var _myType: ITypeRef | Null = null,
               var initExpr: Option[Expr] = None,
               val isFinal: Boolean = false) extends IVariableDecl {
  override def myType: Option[ITypeRef] = Option(_myType)
  override def myType_=(varType: ITypeRef) = _myType = varType

  override def equals(obj: Any): Boolean = obj match {
    case other: LocalVar => name == other.name && myType == other.myType && isFinal == other.isFinal
    case _ => false
  }
  override def text: String = {
    val sb = StringBuilder(if (isFinal) "final " else "").append(name)
    if (initExpr != None) sb.append('=').append(initExpr.get.text).append(';')
    sb.toString
  }
}