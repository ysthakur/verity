package com.ysthakur.parsing.ast.infile.expr

import com.ysthakur.parsing._
import com.ysthakur.parsing.ast._
import com.ysthakur.parsing.ast.infile._
import com.ysthakur.parsing.parser._

case class DotRef(iterable: Iterable[ValidIdNode]) extends Expr {
//  val startOffset: Int = iterable.head.startOffset
//  val endOffset: Int = iterable.last.endOffset
  override def text: String =
    iterable.map(_.text).mkString(".")
  //def this(pattern: PatternWithMatch[RepeatPattern[Node], Match[Node]]) = this(null)
}