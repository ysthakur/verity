package verity.core

import verity.core.*
import verity.util.*

import cats.data.{Writer, OptionT}
import cats.implicits._
import org.junit.Assert
import org.junit.Test
import org.junit.Before
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

  @Test def writerTest() = {
    val optiont1 = OptionT(Writer(List("foo", "bar"), Some(4)))
    val option2 = OptionT(Writer(List("baz"), Some(5)))
    val none = OptionT(Writer(List("blah", "sdfasdfasdf"), None: Option[Int]))

    println(for (o <- optiont1; o2 <- option2) yield o + o2)

    println(for (o <- optiont1; o2 <- none) yield o + o2)

  }
}
