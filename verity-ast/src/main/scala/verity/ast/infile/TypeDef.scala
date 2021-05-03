package verity.ast.infile

import verity.ast.TextRange

trait TypeDef extends NamedTree {
  /**
   * Get a reference to this type parameter
   */
  def makeRef(typeArgs: Seq[Type] = Nil): TypeRef = TypeRef(this.name, typeArgs, TextRange.synthetic, Some(this))
}
