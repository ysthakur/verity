package verity.compiler.core

import verity.compiler.util.unsafeNN

import java.io.File
import java.nio.file.Path

/** Groups all the flags/options
  * @param jdkDir The path to the jdk directory (C:\Program Files\Java\jdk-11.0.5)
  * @param modulesToRead Which modules this depends on ("java.base", etc.)
  * @param javaOutputDir Where the java files will be outputted
  */
case class Options(
  javaOutputDir: File,
  jdkDir: String = System.getProperty("JAVA_HOME", raw"C:\Program Files\Java\jdk-11.0.5").unsafeNN,
  modulesToRead: Seq[String] = Seq("java.base")
)
