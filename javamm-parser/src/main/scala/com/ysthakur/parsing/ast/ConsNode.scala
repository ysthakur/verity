package com.ysthakur.parsing.ast

case class ConsNode[N1 <: Node, N2 <: Node](n1: N1, n2: N2) extends Node