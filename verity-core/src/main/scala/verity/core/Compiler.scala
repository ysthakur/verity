package verity.core

import verity.ast.{Pkg, RootPkg, PkgNode, FileNode, TextRange, HasText}
import verity.checks.InitialPass
import verity.core.resolve
import verity.util.*
import verity.parser.Parser

import com.typesafe.scalalogging.Logger

import java.io.{File, FileInputStream, FileFilter}
import java.nio.file.{Path, Files}
import collection.mutable.ListBuffer

object Compiler {
  def compile(pkgs: Iterable[File], files: Iterable[File], options: Options) = {
    given logger: Logger = Logger("thelogger")
    given rootPkg: RootPkg = RootPkg(ListBuffer.empty, ListBuffer.empty)

    parsePkg(pkgs, files, rootPkg)
    verity.checks.InitialPass.initialPass(rootPkg)
    resolve.resolveAndCheck(rootPkg)
    OutputJava.outputJavaPkg(rootPkg, options.javaOutputDir)
  }

  def logError(msg: String, tree: HasText, file: FileNode)(using logger: Logger): Unit =
    logError(msg, tree.textRange, file)

  def logError(msg: String, pos: TextRange, file: FileNode)(using logger: Logger): Unit =
    logger.error(s"Error: $msg from offset ${pos.start} to ${pos.end} in file ${file.name}")

  def logError(msg: String, pos: TextRange)(using ctxt: Context, logger: Logger): Unit =
    logError(msg, pos, ctxt.file)

  def logError(msg: String, tree: HasText)(using ctxt: Context, logger: Logger): Unit =
    logError(msg, tree.textRange, ctxt.file)

  def logError(errorMsg: ErrorMsg)(using ctxt: Context, logger: Logger): Unit =
    logger.error(
        errorMsg.msg,
        errorMsg.textRangeOrTree match {
          case tr: TextRange => tr
          case ht: HasText   => ht.textRange
        }
    )

  def parsePkg(
      pkgs: Iterable[File],
      files: Iterable[File],
      parent: Pkg
  )(using logger: Logger): Unit = {
    parent.files ++= files
      .map { file =>
        Parser.parseFile(file.getName.unsafeNN, file) match {
          case e @ Left((errorMsg, offset)) =>
            //todo
            logger.error(s"Error while parsing ${file.getName.unsafeNN}: $errorMsg")
            e
          case s => s
        }
      }
      .collect { case Right(file) => file }

    pkgs.foreach { pkg =>
      val pkgNode = PkgNode(pkg.getName.unsafeNN, ListBuffer.empty, ListBuffer.empty, parent)
      parent.subPkgs += pkgNode

      val (subPkgs, allFiles) = pkg
        .listFiles((file => file.isDirectory || file.getName.endsWith(".verity")): FileFilter)
        .unsafeNN
        .view
        .partition(_.isDirectory)

      println(s"subPkgs=$subPkgs, allFiles=$allFiles")

      parsePkg(
          subPkgs.filterNotNull,
          allFiles.filter(file => file != null && file.getName.endsWith(".verity")).removeNull,
          pkgNode
      )
    }
  }
}
