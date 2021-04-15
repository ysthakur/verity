package verity.core

import scala.language.unsafeNulls

import verity.ast.*
import verity.parsing.*
import verity.parsing.parser.Parser
import verity.checks.initial.InitialChecks
import verity.core.references.ReferenceResolve
import verity.util.*

import com.typesafe.scalalogging.Logger

import java.io.{File, FileInputStream, FileFilter}
import java.nio.file.{Path, Files}
import collection.mutable.ListBuffer

def compile(pkgs: Iterable[File], files: Iterable[File], options: Options) = {
  given logger: Logger = Logger("Syntax errors")
  given rootPkg: RootPkg = RootPkg(ListBuffer.empty, ListBuffer.empty)
  
  parsePkg(pkgs, files, rootPkg)
  ReferenceResolve.resolveSimpleReferencesInPkg(rootPkg, Nil)
  OutputJava.outputJavaPkg(rootPkg, options.javaOutputDir)
}

def parsePkg(
  pkgs: Iterable[File],
  files: Iterable[File],
  parent: Package
)(using logger: Logger): Unit = {
  parent.files ++= files
    .map { file =>
      Parser.parseFile(file.getName.unsafeNN, new FileInputStream(file)) match {
        case e @ Left((errorMsg, offset)) =>
          logger.debug(errorMsg) //todo
          e
        case s => s
      }
    }
    .collect {
      case Right(file) =>
        InitialChecks.checkFile(file, logger)
        file
    }

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
