package verity.readbytecode

import scala.language.unsafeNulls

import verity.ast._
import verity.util._

import org.objectweb.asm.ClassReader

import java.io.{File, FileInputStream, IOException}
import java.util.jar.{JarEntry, JarFile}
import scala.collection.mutable

object ReadBytecode {
  def readClassFile(rootPkg: RootPkg, classFile: File): Option[infile.Classlike] = {
//    try {
      readClass(rootPkg, new FileInputStream(classFile))
      println(s"read classfile $classFile")
      None
//    } catch {
//      case (_: IOException) | (_: SecurityException) =>
//        None
//    }
  }

  def readJar(rootPkg: RootPkg, jarFile: File): Option[Pkg] = {
    try {
      val jar = new JarFile(jarFile)
      jar.stream().forEach(jarEntry => readEntry(rootPkg, jar, jarEntry))
      None
    } catch {
      case e => throw e
      case (_: IOException) | (_: SecurityException) =>
        None
    }
  }

  private def readEntry(
    rootPkg: RootPkg,
    jar: JarFile,
    entry: JarEntry
  ): Unit = {
    println(s"reading entry ${entry.getName}")
    if (entry.getName.endsWith(".class")) {
      readClass(rootPkg, jar.getInputStream(entry))
    }
  }

  def readClass(
    rootPkg: RootPkg,
    input: java.io.InputStream
  ): Unit = {
    try {
      val reader = ClassReader(input)
      reader.accept(VerityClassVisitor(rootPkg), ClassReader.SKIP_CODE | ClassReader.SKIP_FRAMES)
    } catch {
      case (_: IllegalArgumentException) | (_: ArrayIndexOutOfBoundsException) =>
    }
  }
}
