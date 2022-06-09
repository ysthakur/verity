package verity.parser

import verity.ast.{FileNode, TextRange, Position}

import fastparse._, JavaWhitespace._

import java.io.{File, FileInputStream, InputStream}
import collection.mutable.ArrayBuffer

private class Parser(inputFile: File)(implicit offsetToPos: ArrayBuffer[(Int, Int, Int)]) {
  val core = new Core
  val types = new Types(core)
  val exprs = new Exprs(core, types)
  val methods = new Methods(core, types, exprs)
  val classlikes = new Classlikes(core, types, exprs, methods)

  def file[_: Parser]: Parser[FileNode] =
    Parser(core.packageStmt.? ~ core.importStmt.rep ~ classlikes.classlike.rep ~ End).map {
      case (pkgStmt, imptStmts, templateDefs) =>
        FileNode(inputFile.getName, pkgStmt, imptStmts, templateDefs, Some(inputFile), offsetToPos)
    }
}

object Parser {
  def parseFile(name: String, input: File): Either[(String, Int), FileNode] = {
    val offsetToPos = ArrayBuffer.empty[(Int, Int, Int)]
    val parser = new Parser(input)(offsetToPos)
    val tis = new TrackingInputStream(new FileInputStream(input), offsetToPos)

    (parse(tis, parser.file(_), verboseFailures = true): @unchecked) match {
      case Parsed.Success(ast, _) => Right(ast)
      case Parsed.Failure(label, index, extra) =>
        Left((makeSyntaxErrorMsg(label, index, extra, offsetToPos), index))
    }
  }

  def makeSyntaxErrorMsg(
    label: String,
    index: Int,
    extra: Parsed.Extra,
    offsetToPos: ArrayBuffer[(Int, Int, Int)]
  ): String =
    s"Syntax error at offset ${extra.index}, rowcol=${FileNode.getPos(offsetToPos, extra.index)}, label = $label, ${extra.stack}"

  private[parser] def getPos(offset: Int)(implicit
    offsetToPos: ArrayBuffer[(Int, Int, Int)]
  ): Position =
    FileNode.getPos(offsetToPos, offset)

  /** Create a [[TextRange]] from two positions
    *
    * @param startOffset
    * @param endOffset
    * @return
    */
  private[parser] def ps2tr(startOffset: Int, endOffset: Int)(implicit
    offsetToPos: ArrayBuffer[(Int, Int, Int)]
  ): TextRange =
    TextRange(getPos(startOffset), getPos(endOffset))

  private class TrackingInputStream(
    delegate: InputStream,
    private[parser] val offsetToPos: ArrayBuffer[(Int, Int, Int)]
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
          offsetToPos += ((offset, row, col))
          reachedEnd = true
        } else if (char == 12 || char == 10 && prevChar != 12) {
          offsetToPos += ((offset, row, col))
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
