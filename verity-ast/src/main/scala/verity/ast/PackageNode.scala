package verity.ast

import collection.mutable.ListBuffer

class PackageNode(private[verity] var _subPkgs: ListBuffer[PackageNode], private[verity] var _files: ListBuffer[FileNode]) {
  def subPkgs: Iterable[PackageNode] = _subPkgs
  def files: Iterable[FileNode] = _files
}
