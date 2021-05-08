package verity.core

import verity.ast.{FileNode, TextRange, HasText}

import com.typesafe.scalalogging.Logger

object LogUtils {
  def logError(msg: String, tree: HasText, file: FileNode)(using logger: Logger): Unit =
    logError(msg, tree.textRange, file)

  def logError(msg: String, pos: TextRange, file: FileNode)(using logger: Logger): Unit =
    logger.error(s"Error: $msg from offset ${pos.start} to ${pos.end} in file ${file.name}")

  def logError(msg: String, pos: TextRange)(using ctxt: Context, logger: Logger): Unit =
    logError(msg, pos, ctxt.file)

  def logError(msg: String, tree: HasText)(using ctxt: Context, logger: Logger): Unit =
    logError(msg, tree.textRange, ctxt.file)

  def log(msg: CompilerMsg, file: FileNode)(using Logger): Unit =
    logError(
      msg.msg,
      msg.textRangeOrTree match {
        case tr: TextRange => tr
        case ht: HasText   => ht.textRange
      },
      file
    )
    
  def log(msg: CompilerMsg)(using ctxt: Context, logger: Logger): Unit =
    log(msg, ctxt.file)
}
