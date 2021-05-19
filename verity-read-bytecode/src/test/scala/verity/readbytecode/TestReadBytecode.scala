package verity.readbytecode

import org.junit.Assert
import org.junit.Test
import org.junit.Before

import java.io.File
import java.net.URI
import java.net.URLClassLoader
import java.nio.file.FileSystem
import java.nio.file.FileSystems
import java.nio.file.Files
import java.nio.file.FileSystemException
import java.util.Collections
import java.nio.file.FileSystems
import java.nio.file.Paths
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

class TestReadBytecode {
  @Test def readClassfile(): Unit = {
    val file =
      java.io.File("C:/Users/yasht/verity/verity-read-bytecode/src/test/resources/TestASM.class")
    val rootPkg = verity.ast.RootPkg(ArrayBuffer.empty, ArrayBuffer.empty)
    ReadBytecode.readClassFile(rootPkg, file)
  }

  @Test def readJar(): Unit = {
    val file = java.io.File(raw"C:\Program Files\Java\jdk-11.0.8\lib\jrt-fs.jar")
    val rootPkg = verity.ast.RootPkg(ArrayBuffer.empty, ArrayBuffer.empty)
    ReadBytecode.readJar(rootPkg, file)
    println(rootPkg.subPkgs)
  }

  @Test def readJdk(): Unit = {
    val rootPkg = verity.ast.RootPkg(ArrayBuffer.empty, ArrayBuffer.empty)
    val p = Paths.get(raw"C:\Program Files\Java\jdk-11.0.8")
    val loader = URLClassLoader(Array(p.toUri.toURL))
    val fs = FileSystems.newFileSystem(URI.create("jrt:/"), Collections.emptyMap, loader)
    try {
      Files.list(fs.getPath("/modules")).forEach { path =>
        Files.walk(path).forEach { classFile =>
          try {
            ReadBytecode.readClass(rootPkg, Files.newInputStream(classFile))
          } catch {
            case e: FileSystemException =>
          }
        }
      }
      println(rootPkg.subPkgs)
    } finally {
      if (fs != null) fs.close()
    }
  }
}
