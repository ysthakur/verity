package com.ysthakur.javamm.parsing.ast.infile.expr

import com.ysthakur.javamm.parsing.ast.infile.ValidIdNode

case class VarRef(varName: ValidIdNode) extends Expr {
    override def startOffset = varName.startOffset
    override def endOffset = varName.endOffset
    override def text = varName.text
}