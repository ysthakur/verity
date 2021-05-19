package verity.core

import verity.core._
import verity.util._

import cats.data.{Writer, OptionT}
import cats.implicits._
import org.junit.{Assert, Test, Before}

import java.io.File

class ParsePkgTest {
  val testDir = raw"C:\Users\yasht\verity\verity-core\src\test\"
  val srcDir = File(testDir + "src")

  @Before def setup() = {
    assert(srcDir.exists)
  }

  @Test def parsePackageTest() = {
    val options = Options(
      javaOutputDir = File(testDir + "javaOutput")
    )
    // val srcDirChildren = srcDir.listFiles.asInstanceOf[Array[File]]

    Compiler.compile(List(File(srcDir, "com")), Nil, options)
  }
}
