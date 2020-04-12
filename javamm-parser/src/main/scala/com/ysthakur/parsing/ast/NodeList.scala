package com.ysthakur.parsing.ast

class NodeList[T <: Node](val nodes: Iterable[T]) extends Node {
}