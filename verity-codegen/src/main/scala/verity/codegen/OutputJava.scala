package verity.codegen

import verity.ast._

import scala.util.Using
import java.io.{File, FileWriter}

object OutputJava {
  def outputJavaPkg(pkg: Pkg, outputDir: File): Unit = {
//    if (!outputDir.exists) outputDir.mkdir()

    /*println(
        s"Outputting package ${pkg.name}, pkgs=${pkg.subPkgs.map(_.name)}, files=${pkg.files.map(_.name)}"
    )*/

    pkg.files.foreach { fileNode =>
      outputJavaFile(
        fileNode,
        outputDir,
        File(outputDir, fileNode.name.replace(".verity", ".java"))
      )
    }
    pkg.subPkgs.foreach { subPkg => outputJavaPkg(subPkg, File(outputDir, subPkg.name)) }
  }

  def outputJavaFile(file: FileNode, outputDir: File, outputFile: File): Unit = {
    if (file.isSource) {
      println(s"Outputting file ${file.name}")
      // println(file.text)
      if (!outputDir.exists) outputDir.mkdir()
      if (!outputFile.exists) outputFile.createNewFile()

      Using(FileWriter(outputFile)) { fw =>
        fw.write(file.text)
      }
    }
  }
}
