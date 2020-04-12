package com.ysthakur.parsing.ast.infile.expr

import com.ysthakur.parsing.ast.infile._
import com.ysthakur.parsing.ast._

case class VarRef(varName: ValidIdNode) extends Expr {
    override def startOffset = varName.startOffset
    override def endOffset = varName.endOffset
    override def text = varName.text
}