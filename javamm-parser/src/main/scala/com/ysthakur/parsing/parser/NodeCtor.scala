package com.ysthakur.parsing.parser

import com.ysthakur.parsing.ast.Types._
import com.ysthakur.parsing.grammar.Match

trait NodeCtor[Ma <: Match[Node], N <: Node] extends com.ysthakur.util.Ctor[Node, N] {
  type M = Ma
  def ctor(m: Ma): N
}