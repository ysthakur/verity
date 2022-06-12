package verity.readbytecode

import scala.language.unsafeNulls
import verity.ast._
import verity.util._
import org.objectweb.asm.ClassReader

import java.io.{File, FileInputStream, IOException}
import java.net.{URI, URLClassLoader}
import java.nio.file.{Files, FileSystems, Path, Paths}
import java.util.jar.{JarEntry, JarFile}
import java.util.Collections
import scala.collection.mutable.ArrayBuffer

object ReadBytecode {
  def readClassFile(rootPkg: Package, classFile: File): Unit = {
//    try {
    readClass(rootPkg, new FileInputStream(classFile))
//    } catch {
//      case (_: IOException) | (_: SecurityException) =>
//        None
//    }
  }

  private def readClass(
    rootPkg: Package,
    input: java.io.InputStream
  ): Unit = {
    try {
      val reader = ClassReader(input)
      reader.accept(VerityClassVisitor(rootPkg), ClassReader.SKIP_CODE | ClassReader.SKIP_FRAMES)
    } catch {
      case (_: IllegalArgumentException) | (_: ArrayIndexOutOfBoundsException) =>
    }
  }

  def readJar(rootPkg: Package, jarFile: File): Unit = {
    try {
      val jar = new JarFile(jarFile)
      jar.stream().forEach(jarEntry => readEntry(rootPkg, jar, jarEntry))
    } catch {
      case (_: IOException) | (_: SecurityException) =>
    }
  }

  private def readEntry(
    rootPkg: Package,
    jar: JarFile,
    entry: JarEntry
  ): Unit = {
    if (entry.getName.endsWith(".class")) {
      readClass(rootPkg, jar.getInputStream(entry))
    }
  }

  /** @param rootPkg The root package to which read packages and classes will be added
    * @param jdkPath The path to the jdk directory (C:\Program Files\Java\jdk-11.0.5)
    * @param modules The list of modules to read ("java.base", etc.)
    */
  def readJdk(rootPkg: Package, jdkPath: Path, modules: Seq[String]): Unit = {
    val loader = URLClassLoader(Array(jdkPath.toUri.toURL))
    val fs = FileSystems.newFileSystem(URI.create("jrt:/"), Collections.emptyMap, loader)

    try {
      Files.list(fs.getPath("/modules")).forEach { module =>
        if (modules.contains(module.getFileName.toString)) {
          Files.walk(module).forEach { classFile =>
            if (
              !Files.isDirectory(classFile) && classFile.getFileName.toString != "module-info.class"
            )
              ReadBytecode.readClass(rootPkg, Files.newInputStream(classFile))
          }
        }
      }
    } finally {
      fs.close()
      loader.close()
    }
  }
}
