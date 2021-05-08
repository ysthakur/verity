package verity.core

import verity.ast.{TextRange, HasText}

import cats.data.{OptionT, Writer}

type ResolveResult[T] = OptionT[[T] =>> Writer[List[CompilerMsg], T], T]
// Writer[List[CompilerMsg], Option[T]]
// Option[T]
// Either[ErrorMsg, T]

case class CompilerMsg(msg: String, textRangeOrTree: TextRange | HasText, msgType: MsgType)

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
