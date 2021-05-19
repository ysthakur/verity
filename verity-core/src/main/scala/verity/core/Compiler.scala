package verity.core

import verity.ast._
import verity.checks.InitialPass
import verity.core.resolve
import verity.codegen.OutputJava
import verity.util._
import verity.parser.Parser
import verity.readbytecode.ReadBytecode

import com.typesafe.scalalogging.Logger

import java.io.{File, FileFilter, FileInputStream}
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable.ArrayBuffer

object Compiler {
  def compile(pkgs: Iterable[File], files: Iterable[File], options: Options): Unit = {
    given Logger = Logger(".")
    given rootPkg: RootPkg = RootPkg(ArrayBuffer.empty, ArrayBuffer.empty)

    //Load JDK classes such as java.lang.Object
    ReadBytecode.readJdk(rootPkg, Paths.get(options.jdkDir).unsafeNN, options.modulesToRead)

    //Parse Verity sources
    parsePkg(pkgs, files, rootPkg)

    //Do some initial checks (see if modifiers are okay, resolve imports, references to types, etc.)
    verity.checks.InitialPass.initialPass(rootPkg)
    //Resolve expressions inside methods
    resolve.resolveAndCheck(rootPkg)

    //Code generation
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
