package com.ysthakur.parsing.ast.infile.expr

import com.ysthakur.parsing._
import com.ysthakur.parsing.parser._
import com.ysthakur.parsing.ast._

case class DotRef(iterable: Iterable[VarRef]) extends Expr {
  val startOffset: Int = iterable.head.startOffset
  val endOffset: Int = iterable.last.endOffset
  override def text: StringBuilder =
    iterable.foldRight(StringBuilder()) 
      {(ref, sb) => sb.append(ref.text.toString).append('.')}
  //def this(pattern: PatternWithMatch[RepeatPattern[Node], Match[Node]]) = this(null)
}