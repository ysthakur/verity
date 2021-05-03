package verity.ast

trait Tree {
  def synthetic: Boolean = false
}

object Tree {}

trait Synthetic extends Tree, HasText {
  override def textRange = TextRange(-1, -1)
  override def synthetic = true
}

trait ParentNode extends Tree {
  type Child <: Tree
  def children: Iterable[Child]
}

trait ChildNode extends Tree {
  type Parent <: ParentNode
  def parent: Parent
}

trait NamedTree extends Tree {
  def name: String
}
