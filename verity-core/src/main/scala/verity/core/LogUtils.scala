package verity.core

import verity.ast.{FileNode, HasText, HasTextRange, TextRange}
//import com.typesafe.scalalogging.Logger

object LogUtils {
  def logMsg(msg: String, pos: TextRange | HasTextRange, file: FileNode): Unit =
    val (start, end) = getRowColRange(pos, file)
    println(s"$msg (from $start to $end in file ${file.name})")

  def logMsg(msg: String, pos: TextRange)(using ctxt: Context): Unit =
    logMsg(msg, pos, ctxt.file)

  def log(msg: CompilerMsg, file: FileNode): Unit =
    logMsg(msg.toString, msg.textRangeOrTree, file)

  def log(msg: CompilerMsg)(using ctxt: Context): Unit =
    log(msg, ctxt.file)

  def getRowColRange(textRangeOrTree: TextRange | HasTextRange, file: FileNode) =
    val default = (-1, -1)
    textRangeOrTree match {
      case tr: TextRange =>
        (file.getRowCol(tr.start).getOrElse(default), file.getRowCol(tr.end).getOrElse(default))
      case htr: HasTextRange =>
        (
          file.getRowCol(htr.textRange.start).getOrElse(default),
          file.getRowCol(htr.textRange.end).getOrElse(default)
        )
    }
}
