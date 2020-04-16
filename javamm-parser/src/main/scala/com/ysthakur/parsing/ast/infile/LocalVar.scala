package com.ysthakur.parsing.ast.infile
import com.ysthakur.parsing.ast.infile.expr.Expr

class LocalVar(override val name: String,
               typeRef: Option[TypeRef] = None,
               var varType: Option[TypeRef],
               var initExpr: Option[Expr] = None,
               val isFinal: Boolean = false) extends IVariableDecl {
//  private var varType_ : Option[TypeRef] = typeRef
//  override def varType: Option[TypeRef] = this.varType_
//  /*private[ysthakur]*/ def varType_=(varType: Option[TypeRef]) = varType_ = typeRef
  override def equals(obj: Any): Boolean = obj match {
    case other: LocalVar => name == other.name && varType == other.varType && isFinal == other.isFinal
    case _ => false
  }
}
