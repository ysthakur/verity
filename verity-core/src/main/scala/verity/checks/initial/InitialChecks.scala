package verity.checks.initial

import verity.ast.*
import verity.ast.infile.*
import verity.parsing.*
import verity.util.*

import com.typesafe.scalalogging.Logger

object InitialChecks {
  def checkFile(fileNode: FileNode, logger: Logger): Unit = {

  }

  def verifyPkgStmt(pkgRef: Option[PackageStmt], pkgName: String, fileName: String)(using logger: Logger) =
    pkgRef match {
      case Some(pkgStmt) =>
        val foundText = pkgStmt.path.text
        if (foundText != pkgName)
          if (pkgName.nonEmpty)
            logger.error(
                s"Wrong package statement in file ${fileName}, should be $pkgName, found $foundText"
            )
      case None =>
        if (pkgName.nonEmpty)
          logger.error(s"No package statement in file ${fileName} in package $pkgName")
    }
}