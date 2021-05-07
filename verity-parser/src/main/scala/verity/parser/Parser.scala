package verity.parser

import verity.ast.FileNode

import fastparse._, JavaWhitespace._

import java.io.{File, FileInputStream}

object Parser {
  
  def parseFile(name: String, input: File): Either[(String, Int), FileNode] =
    (parse(new FileInputStream(input), file(input)(_), verboseFailures=true): @unchecked) match {
      case Parsed.Success(ast, _) => Right(ast)
      case Parsed.Failure(label, index, extra) => Left((makeSyntaxErrorMsg(label, index, extra), index))
    }

  def makeSyntaxErrorMsg(label: String, index: Int, extra: Parsed.Extra): String =
    s"Syntax error at offset ${extra.index}, label = $label, ${extra.stack}"

  def file[_: P](file: File): P[FileNode] = P(Core.packageStmt.? ~ Core.importStmt.rep ~ Classlikes.classlike.rep ~ End).map {
    case (pkgStmt, imptStmts, templateDefs) => FileNode(file.getName, pkgStmt, imptStmts, templateDefs, file)
  }
}
