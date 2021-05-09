package verity.readbytecode

import scala.language.unsafeNulls

import verity.util._
import verity.ast.{RootPkg, Pkg, PkgNode}

import org.objectweb.asm.{ClassReader}

import java.io.{File, IOException}
import java.util.jar.{JarFile, JarEntry}

object ReadBytecode {
  def readJar(jarFile: File): Option[Pkg] = {
    try {
      val jar = new JarFile(jarFile)
      jar.stream().map(jarEntry => readEntry(jar, jarEntry))
      ???
    } catch {
      case (_: IOException) | (_: SecurityException) =>
        None
    }
  }

  def readEntry(jar: JarFile, entry: JarEntry) = {
    try {
      val entryName = entry.getName
      Option.when(entryName.endsWith(".class").unsafeNN) {
        val jis = jar.getInputStream(entry)
        val reader = ClassReader(jis)
        reader.accept(VerityClassVisitor(), ClassReader.SKIP_CODE | ClassReader.SKIP_FRAMES)
      }
    } catch {
      case (_: IOException) =>
        None
    }
  }
}
