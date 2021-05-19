package verity.readbytecode

import scala.language.unsafeNulls

import verity.ast._
import verity.util._

import org.objectweb.asm.ClassReader

import java.io.{File, FileInputStream, IOException}
import java.util.jar.{JarEntry, JarFile}
import scala.collection.mutable

object ReadBytecode {
  def readJar(rootPkg: RootPkg, jarFile: File): Option[Pkg] = {
    try {
      val jar = new JarFile(jarFile)
      val classMap = mutable.HashMap[String, infile.Classlike]()
      jar.stream().map(jarEntry => readEntry(rootPkg, jar, jarEntry, classMap))
      ???
    } catch {
      case (_: IOException) | (_: SecurityException) =>
        None
    }
  }

  private def readEntry(
    rootPkg: RootPkg,
    jar: JarFile,
    entry: JarEntry,
    classMap: mutable.HashMap[String, infile.Classlike]
  ) = {
    if (entry.getName.endsWith(".class")) {
      readInputStream(rootPkg, jar.getInputStream(entry), classMap)
    }
  }

  def readClassFile(rootPkg: RootPkg, classFile: File): Option[infile.Classlike] = {
    try {
      val classMap = mutable.HashMap[String, infile.Classlike]()
      readInputStream(rootPkg, new FileInputStream(classFile), classMap)
      None
    } catch {
      case (_: IOException) | (_: SecurityException) =>
        None
    }
  }

  private def readInputStream(
    rootPkg: RootPkg,
    input: java.io.InputStream,
    classMap: mutable.HashMap[String, infile.Classlike]
  ) = {
    try {
      val reader = ClassReader(input)
      reader.accept(VerityClassVisitor(rootPkg), ClassReader.SKIP_CODE | ClassReader.SKIP_FRAMES)
    } catch {
      case e: IOException => e.printStackTrace
    }
  }
}
