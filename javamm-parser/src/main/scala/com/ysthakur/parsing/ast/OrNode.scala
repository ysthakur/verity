package com.ysthakur.parsing.ast

sealed trait OrNode[L <: Node, R <: Node]

case class LeftNode[L <: Node, R <: Node](left: L) extends OrNode[L, R]
case class RightNode[L <: Node, R <: Node](right: R) extends OrNode[L, R]