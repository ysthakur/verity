package com.ysthakur.parsing.parser

import com.ysthakur.parsing.grammar.Match
import com.ysthakur.util.Ctor

trait NodeCtor[M <: Match[Input], override type This, type Input] extends Ctor[M, T] {
  override type In = M
}
