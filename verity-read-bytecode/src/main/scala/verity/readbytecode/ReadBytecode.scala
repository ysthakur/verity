package verity.readbytecode

import scala.language.unsafeNulls

import verity.util._
import verity.ast.{RootPkg, Pkg, PkgNode}
import verity.ast.infile

import org.objectweb.asm.{ClassReader}

import java.io.{File, IOException}
import java.util.jar.{JarFile, JarEntry}
import scala.collection.mutable.HashMap

object ReadBytecode {
  def readJar(jarFile: File): Option[Pkg] = {
    try {
      val jar = new JarFile(jarFile)
      val classMap = HashMap[String, infile.Classlike]()
      jar.stream().map(jarEntry => readEntry(jar, jarEntry, classMap))
      ???
    } catch {
      case (_: IOException) | (_: SecurityException) =>
        None
    }
  }

  def readEntry(jar: JarFile, entry: JarEntry, classMap: HashMap[String, infile.Classlike]) = {
    try {
      val entryName = entry.getName
      Option.when(entryName.endsWith(".class")) {
        val jis = jar.getInputStream(entry)
        val reader = ClassReader(jis)
        reader.accept(VerityClassVisitor(classMap), ClassReader.SKIP_CODE | ClassReader.SKIP_FRAMES)
      }
    } catch {
      case (_: IOException) =>
        None
    }
  }
}
