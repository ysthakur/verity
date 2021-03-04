package verity.ast

trait Tree {
  def as[T]: T = asInstanceOf[T]
  def flatten: Tree = this
}

trait ParentNode extends Tree {
  type Child <: Tree
  def children: Iterable[Child]
}

trait ChildNode extends Tree {
  type Parent <: ParentNode
  def parent: Parent
}
