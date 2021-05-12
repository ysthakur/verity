package verity.readbytecode

import scala.language.unsafeNulls
import verity.util._
import verity.ast.Pkg
import verity.ast.PkgNode
import verity.ast.RootPkg
import verity.ast.infile
import org.objectweb.asm.ClassReader

import java.io.File
import java.io.FileInputStream
import java.io.IOException
import java.util.jar.JarEntry
import java.util.jar.JarFile
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

object ReadBytecode {
  def readJar(jarFile: File): Option[Pkg] = {
    try {
      val jar = new JarFile(jarFile)
      val classMap = mutable.HashMap[String, infile.Classlike]()
      val counter = AtomicInteger(0)
      jar.stream().map(jarEntry => readEntry(jar, jarEntry, classMap, counter))
      ???
    } catch {
      case (_: IOException) | (_: SecurityException) =>
        None
    }
  }

  def readClassFile(classFile: File): Option[infile.Classlike] = {
    try {
      val classMap = mutable.HashMap[String, infile.Classlike]()
      val counter = AtomicInteger(0)
      readInputStream(new FileInputStream(classFile), classMap, counter)
      ???
    } catch {
      case (_: IOException) | (_: SecurityException) =>
        None
    }
  }

  private def readEntry(
    jar: JarFile,
    entry: JarEntry,
    classMap: mutable.HashMap[String, infile.Classlike],
    counter: AtomicInteger
  ) = {
    if (entry.getName.endsWith(".class")) {
      readInputStream(jar.getInputStream(entry), classMap, counter)
    }
  }

  private def readInputStream(
    input: java.io.InputStream,
    classMap: mutable.HashMap[String, infile.Classlike],
    counter: AtomicInteger
  ) = {
    try {
      val reader = ClassReader(input)
      reader.accept(VerityClassVisitor(classMap, counter), ClassReader.SKIP_CODE | ClassReader.SKIP_FRAMES)
    } catch {
      case e: IOException => e.printStackTrace
    }
  }
}
