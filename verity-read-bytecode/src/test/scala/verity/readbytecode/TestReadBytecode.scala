package verity.readbytecode

import org.junit.{Assert, Before, Test}

import java.io.File
import java.net.{URI, URLClassLoader}
import java.util.Collections
import java.nio.file.{Files, FileSystem, FileSystemException, FileSystems, Paths}
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

class TestReadBytecode {
  var homePath: String = _
  var jdkPath: String = _
  val S = File.separator

  @Before def findOS(): Unit = {
    val os = System.getProperty("os.name")
    val isWindows = os.startsWith("Windows")
    homePath = if (isWindows) "C:/Users/yasht" else "~"
    jdkPath =
      if (isWindows) raw"""C:\Program Files\Java\jdk-11.0.8""" else "/usr/lib/jvm/default-java"
  }

  @Test def readClassfile(): Unit = {
    val file =
      java.io.File(
        Seq(
          "verity-read-bytecode",
          "src",
          "test",
          "resources",
          "TestASM.class"
        ).mkString(File.separator)
      )
    val rootPkg = verity.ast.Package(ArrayBuffer.empty, ArrayBuffer.empty)
    ReadBytecode.readClassFile(rootPkg, file)
  }

  @Test def readJar(): Unit = {
    val file = java.io.File(s"$jdkPath${S}lib${S}jrt-fs.jar")
    val rootPkg = verity.ast.Package(ArrayBuffer.empty, ArrayBuffer.empty)
    ReadBytecode.readJar(rootPkg, file)
  }

  @Test def readJdk(): Unit = {
    val rootPkg = verity.ast.Package(ArrayBuffer.empty, ArrayBuffer.empty)
    val p = Paths.get(jdkPath)
    ReadBytecode.readJdk(rootPkg, p, Seq("java.base"))
    assert(
      rootPkg.subPkgs
        .find(_.name == "java")
        .get
        .subPkgs
        .find(_.name == "lang")
        .get
        .files
        .exists(_.classlikes.exists(_.name == "String"))
    )
//    println(rootPkg.subPkgs.flatMap(p => p.subPkgs.map(p.name + "." + _.name)))
  }
}
