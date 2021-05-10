package verity.core

import verity.ast.{FileNode, TextRange, HasText}

import com.typesafe.scalalogging.Logger

object LogUtils {
  def logMsg(msg: String, tree: HasText, file: FileNode)(using logger: Logger): Unit =
    logMsg(msg, tree.textRange, file)

  def logMsg(msg: String, pos: TextRange, file: FileNode)(using logger: Logger): Unit =
    println(s"$msg (from offset ${pos.start} to ${pos.end} in file ${file.name})")

  def logMsg(msg: String, pos: TextRange)(using ctxt: Context, logger: Logger): Unit =
    logMsg(msg, pos, ctxt.file)

  def logMsg(msg: String, tree: HasText)(using ctxt: Context, logger: Logger): Unit =
    logMsg(msg, tree.textRange, ctxt.file)

  def log(msg: CompilerMsg, file: FileNode)(using Logger): Unit =
    logMsg(
      msg.toString,
      msg.textRangeOrTree match {
        case tr: TextRange => tr
        case ht: HasText   => ht.textRange
      },
      file
    )
    
  def log(msg: CompilerMsg)(using ctxt: Context, logger: Logger): Unit =
    log(msg, ctxt.file)
}
