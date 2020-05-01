package com.ysthakur.javamm.parsing.ast.infile

import com.ysthakur.javamm.parsing.ast.ParentNode

import scala.collection.mutable.ListBuffer

case class TypeDef(
    modifiers: ModifierList,
    metaclass: TypeDefType,
    name: String,
    override val children: ListBuffer[Field | Method | Rule]
) extends Node
    with ParentNode
    with HasModifiers
    with TypeRepr {
  override type Child = Field | Method | Rule
  override def text: String =
    s"${modifiers.text} ${metaclass.text} $name { ${children.map(_.text).mkString(" ")}}"
}

enum TypeDefType(override val text: String) extends Node {
  case CLASS extends TypeDefType("class")
  case INTERFACE extends TypeDefType("interface")
  case ENUM extends TypeDefType("enum")
}