package verity.core

import verity.core.*
import verity.util.*

import org.junit.Assert
import org.junit.Test
import org.junit.Before
import java.io.File

class ParsePackageTest {
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
