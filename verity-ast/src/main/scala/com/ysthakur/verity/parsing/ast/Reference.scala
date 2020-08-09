package com.ysthakur.verity.parsing.ast

trait Reference[+T <: INode] {
  def resolve: Option[T]
}