package verity.ast

import collection.mutable.ListBuffer

class PackageNode private(
  val name: String,
  private[verity] val _subPkgs: ListBuffer[PackageNode],
  private[verity] val _files: ListBuffer[FileNode]
) {
  def subPkgs: Iterable[PackageNode] = _subPkgs
  def files: Iterable[FileNode] = _files

  def this(name: String, subPkgs: Iterable[PackageNode], files: Iterable[FileNode]) = this(name, subPkgs.to(ListBuffer), files.to(ListBuffer))
}
