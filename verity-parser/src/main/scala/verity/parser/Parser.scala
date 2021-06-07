package verity.parser

import verity.ast.FileNode

import fastparse._, JavaWhitespace._

import java.io.{File, FileInputStream, InputStream}
import collection.mutable.ArrayBuffer

object Parser {
  
  def parseFile(name: String, input: File): Either[(String, Int), FileNode] = {
    val offsetToRowCol = ArrayBuffer.empty[(Int, Int, Int)]
    val res = parse(new TrackingInputStream(new FileInputStream(input), offsetToRowCol), file(input, offsetToRowCol)(_), verboseFailures=true)
    (res: @unchecked) match {
      case Parsed.Success(ast, _) => Right(ast)
      case Parsed.Failure(label, index, extra) => Left((makeSyntaxErrorMsg(label, index, extra, offsetToRowCol), index))
    }
  }

  def makeSyntaxErrorMsg(label: String, index: Int, extra: Parsed.Extra, offsetToRowCol: ArrayBuffer[(Int, Int, Int)]): String =
    s"Syntax error at offset ${extra.index}, rowcol=${FileNode.getRowCol(offsetToRowCol, extra.index)}, label = $label, ${extra.stack}"

  def file[_: P](file: File, offsetToRowCol: ArrayBuffer[(Int, Int, Int)]): P[FileNode] =
    P(Core.packageStmt.? ~ Core.importStmt.rep ~ Classlikes.classlike.rep ~ End).map {
      case (pkgStmt, imptStmts, templateDefs) =>
        FileNode(file.getName, pkgStmt, imptStmts, templateDefs, Some(file), offsetToRowCol)
    }

  class TrackingInputStream(
    delegate: InputStream,
    private[parser] val offsetToRowCol: ArrayBuffer[(Int, Int, Int)]
  ) extends InputStream {
    private var offset: Int = 0
    private var row: Int = 1
    private var col: Int = 1
    private var prevChar: Int = 0
    private var reachedEnd: Boolean = false

    override def read() = {
      if (reachedEnd) {
        -1
      } else {
        val char = delegate.read()
        offset += 1
        if (char == -1) {
          offsetToRowCol += ((offset, row, col))
          reachedEnd = true
        } else if (char == 12 || char == 10 && prevChar != 12) {
          offsetToRowCol += ((offset, row, col))
          row += 1
          col = 0
        } else if (char != 10) {
          col += 1
        }

        prevChar = char

        char
      }
    }
  }
}
