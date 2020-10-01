package com.ysthakur.verity.parsing.ast.infile

import com.ysthakur.verity.parsing.TextRange
import com.ysthakur.verity.parsing.ast.infile.expr.Expr

class LocalVar(override val name: String,
               override val textRange: TextRange,
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