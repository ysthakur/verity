package verity.core

import verity.core._
import verity.util._

import cats.data.{Writer, OptionT}
import cats.implicits._
import org.junit.{Assert, Test, Before}

import java.io.File

class CompilePkgTest {
  val testDir = File(Seq("verity-core", "src", "test").mkString(File.separator.unsafeNN))
  val srcDir = File(testDir, "resources")
  val jdkPath = System.getProperty("JAVA_HOME", raw"C:\Program Files\Java\jdk-11.0.5").unsafeNN

  @Before def setup() = {
    assert(srcDir.exists)
  }

  @Test def compilePackageTest() = {
    val options = Options(
      jdkDir = jdkPath,
      modulesToRead = Seq("java.base"),
      javaOutputDir = File(testDir, "javaOutput")
    )
    // val srcDirChildren = srcDir.listFiles.asInstanceOf[Array[File]]

    Compiler.compile(List(File(srcDir, "com")), Nil, options)
  }
}
