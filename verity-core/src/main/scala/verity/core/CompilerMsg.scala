package verity.core

import verity.ast.{TextRange, HasText}

import cats.data.{OptionT, Writer}

type ResultWithLogs[T] = Writer[List[CompilerMsg], T]
type ResolveResult[T] = OptionT[ResultWithLogs, T]

case class CompilerMsg(msg: String, textRangeOrTree: TextRange | HasText, msgType: MsgType) {
  override def toString = s"$msgType: $msg"
}

enum MsgType {
  case ERROR, WARNING, WEAK_WARNING, INFO
}
def errorMsg(msg: String, textRangeOrTree: TextRange | HasText) =
  CompilerMsg(msg, textRangeOrTree, MsgType.ERROR)
def warningMsg(msg: String, textRangeOrTree: TextRange | HasText) =
  CompilerMsg(msg, textRangeOrTree, MsgType.WARNING)
def weakWarningMsg(msg: String, textRangeOrTree: TextRange | HasText) =
  CompilerMsg(msg, textRangeOrTree, MsgType.WEAK_WARNING)
def infoMsg(msg: String, textRangeOrTree: TextRange | HasText) =
  CompilerMsg(msg, textRangeOrTree, MsgType.INFO)

def singleMsg[T](msg: CompilerMsg): ResolveResult[T] =
  OptionT(Writer(msg :: Nil, None))
//  Writer(msg :: Nil, None)
