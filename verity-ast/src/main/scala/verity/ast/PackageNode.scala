package verity.ast

import collection.mutable.ListBuffer

class PackageNode private(private[verity] val _subPkgs: ListBuffer[PackageNode], private[verity] val _files: ListBuffer[FileNode]) {
  def subPkgs: Iterable[PackageNode] = _subPkgs
  def files: Iterable[FileNode] = _files

  def this(subPkgs: Iterable[PackageNode], files: Iterable[FileNode]) = this(subPkgs.to(ListBuffer), files.to(ListBuffer))
}
