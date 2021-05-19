package verity.core

import verity.ast.{Pkg, RootPkg, PkgNode, FileNode, TextRange, HasText}
import verity.checks.InitialPass
import verity.core.resolve
import verity.codegen.OutputJava
import verity.util._
import verity.parser.Parser

import com.typesafe.scalalogging.Logger

import java.io.{File, FileInputStream, FileFilter}
import java.nio.file.{Path, Files}
import scala.collection.mutable.ArrayBuffer

object Compiler {
  def compile(pkgs: Iterable[File], files: Iterable[File], options: Options): Unit = {
    given logger: Logger = Logger(".")
    given rootPkg: RootPkg = RootPkg(ArrayBuffer.empty, ArrayBuffer.empty)

    parsePkg(pkgs, files, rootPkg)
    verity.checks.InitialPass.initialPass(rootPkg)
    resolve.resolveAndCheck(rootPkg)
    OutputJava.outputJavaPkg(rootPkg, options.javaOutputDir)
  }

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
      val pkgNode = PkgNode(pkg.getName.unsafeNN, ArrayBuffer.empty, ArrayBuffer.empty, parent)
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
