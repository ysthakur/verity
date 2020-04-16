package com.ysthakur.parsing.ast

trait Reference[T <: Node] {
  def resolve: T
}