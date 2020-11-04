package verity.parsing.ast.infile

import verity.parsing.ast.Reference

trait ITypeRef extends Node {

}

case class TypeRef() extends Reference[TypeDef] {
  override def resolve: Option[TypeDef] = ???
}
