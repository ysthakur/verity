package com.ysthakur.parsing.ast.infile.expr

import com.ysthakur.parsing.ast.Types._

case class VarRef(varName: ValidIdNode) extends Expr with TextNode {
    override def startOffset = varName.startOffset
    override def endOffset = varName.endOffset
    override def text = varName.text
}