package verity.core

import verity.ast.{FileNode, HasText, HasTextRange, TextRange}
//import com.typesafe.scalalogging.Logger

object LogUtils {
  def logMsg(msg: String, pos: TextRange | HasTextRange, file: FileNode): Unit =
    val TextRange(start, end) = getPosRange(pos, file)
    println(s"$msg (from $start to $end in file ${file.name})")

  def logMsg(msg: String, pos: TextRange)(using ctxt: Context): Unit =
    logMsg(msg, pos, ctxt.file)

  def log(msg: CompilerMsg, file: FileNode): Unit =
    logMsg(msg.toString, msg.textRangeOrTree, file)

  def log(msg: CompilerMsg)(using ctxt: Context): Unit =
    log(msg, ctxt.file)

  def getPosRange(textRangeOrTree: TextRange | HasTextRange, file: FileNode) =
    textRangeOrTree match {
      case tr: TextRange => tr
      case htr: HasTextRange => htr.textRange
    }
}
