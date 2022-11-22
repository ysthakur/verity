package verity.compiler

import verity.compiler.ast.FileNode

import scala.collection.mutable.ArrayBuffer
import cats.data.NonEmptyList

/** Represents a package */
class VerityPkg(val name: String) {
  private val children = ArrayBuffer.empty[VerityPkg]
  private val files = ArrayBuffer.empty[FileNode]

  def addFile(file: FileNode): Unit =
    files += file

  /** Find a child given its path relative to the current package. If the
    * package doesn't exist, creates one
    */
  def getPackageRelative(path: NonEmptyList[String]): VerityPkg = {
    val NonEmptyList(head, tail) = path

    children.find(_.name == head) match {
      case Some(child) =>
        tail match {
          case next :: rest => this.getPackageRelative(NonEmptyList(next, rest))
          case Nil          => child
        }
      case None =>
        val newChild = VerityPkg(head)
        tail match {
          case next :: rest =>
            newChild.getPackageRelative(NonEmptyList(next, rest))
          case Nil => newChild
        }
    }
  }
}
