package verity.core

import java.io.File

/**
 * Groups all the flags/options
 * @param javaOutputDir Where the java files will be outputted
 */
case class Options(
  javaOutputDir: File
)