//package com.ysthakur.javamm.parsing
//
//import com.ysthakur.javamm.parsing.ast.{NodeList, Node}
//
//trait TextGetter[T] {
//  def getText(node: T): String
//}
//
//given TextGetter[NodeList[Node with HasText]] {
//  override def getText[T <: Node with HasText](node: NodeList[T]): String = {
//    val x = implicitly[TextGetter[T]]
//    ""
//  }
//}
