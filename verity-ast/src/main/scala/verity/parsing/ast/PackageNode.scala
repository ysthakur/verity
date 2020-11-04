package verity.parsing.ast

case class PackageNode() extends ParentNode {
  override type Child = FileNode | PackageNode
  override lazy val children: Iterable[Child] = ???
}

abstract class Directory extends INode
