package com.ysthakur.javamm.parsing.ast

trait Reference[+T <: INode] {
  def resolve: Option[T]
}