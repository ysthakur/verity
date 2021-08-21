package verity.core

import verity.ast.{HasTextRange, TextRange}

import cats.data.{OptionT, Writer}

type ResultWithLogs[T] = Writer[List[CompilerMsg], T]
type ResolveResult[T] = OptionT[ResultWithLogs, T]

// case class ResWithLogs[T](res: T, logs: Seq[CompilerMsg]) {
//   def map[R](fn: T => R): ResWithLogs[R] = ResWithLogs(fn(res), logs)
//   def flatMap[R](fn: T => ResWithLogs[R]) = {
//     val ResWithLogs(res2, logs2) = fn(res)
//     ResWithLogs(res2, logs ++ logs2)
//   }
//   def mapLogs(fn: Seq[CompilerMsg] => Seq[CompilerMsg]) {
//     ResWithLogs()
//   }
// }

// opaque type ResolveRes[T] = ResWithLogs[Option[T]]

// extension [T](res: ResolveRes[T]) {

// }

case class CompilerMsg(msg: String, textRangeOrTree: TextRange | HasTextRange, msgType: MsgType) {
  override def toString = s"$msgType: $msg"
}

enum MsgType {
  case ERROR, WARNING, WEAK_WARNING, INFO
}

def errorMsg(msg: String, textRangeOrTree: TextRange | HasTextRange) =
  CompilerMsg(msg, textRangeOrTree, MsgType.ERROR)
def warningMsg(msg: String, textRangeOrTree: TextRange | HasTextRange) =
  CompilerMsg(msg, textRangeOrTree, MsgType.WARNING)
def weakWarningMsg(msg: String, textRangeOrTree: TextRange | HasTextRange) =
  CompilerMsg(msg, textRangeOrTree, MsgType.WEAK_WARNING)
def infoMsg(msg: String, textRangeOrTree: TextRange | HasTextRange) =
  CompilerMsg(msg, textRangeOrTree, MsgType.INFO)

def singleMsg[T](msg: CompilerMsg): ResolveResult[T] =
  OptionT(Writer(msg :: Nil, None))
//  Writer(msg :: Nil, None)
