// package verity.compiler.core

// import verity.compiler.ast.{FileNode, Span}
// //import com.typesafe.scalalogging.Logger

// class Messages private (
//   private[core] val file: FileNode,
//   private[core] val msgs: collection.mutable.ArrayBuffer[CompilerMsg]
// )

// object Messages {
//   def apply(file: FileNode): Messages = new Messages(file, collection.mutable.ArrayBuffer())
// }

// object LogUtils {
//   def logMsg(msg: String, pos: Span, file: FileNode): Unit =
//     val Span(start, end) = getPosRange(pos, file)
//     println(s"$msg (from $start to $end in file ${file.name})")

//   def logMsg(msg: String, pos: Span)(using ctxt: Context): Unit =
//     logMsg(msg, pos, ctxt.file)

//   def log(msg: CompilerMsg, file: FileNode): Unit =
//     logMsg(msg.toString, msg.textRangeOrTree, file)

//   def log(msg: CompilerMsg)(using ctxt: Context): Unit =
//     log(msg, ctxt.file)

//   def getPosRange(textRangeOrTree: Span | HasTextRange, file: FileNode) =
//     textRangeOrTree match {
//       case span: Span     => span
//       case htr: HasTextRange => htr.span
//     }
// }
