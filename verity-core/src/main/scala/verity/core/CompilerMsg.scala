package verity.core

import verity.ast.TextRange
import cats.data.{OptionT, Writer}

//type ResWithLogs[T] = Writer[List[CompilerMsg], T]
//type ResolveRes[T] = OptionT[ResWithLogs, T]

case class ResLogged[T](value: T, logs: Seq[CompilerMsg]) {
  def map[R](fn: T => R): ResLogged[R] = ResLogged(fn(value), logs)

  def flatMap[R](fn: T => ResLogged[R]): ResLogged[R] = {
    val ResLogged(res2, logs2) = fn(value)
    ResLogged(res2, logs ++ logs2)
  }

  def mapLogs(fn: Seq[CompilerMsg] => Seq[CompilerMsg]): ResLogged[T] = {
    ResLogged(value, fn(logs))
  }
}

extension [T](res: ResLogged[Option[T]]) def toResolveRes: ResolveRes[T] = res

opaque type ResolveRes[T] = ResLogged[Option[T]]

object ResolveRes {
  def apply[T](realRes: ResLogged[Option[T]]): ResolveRes[T] = realRes

  def fromRes[T](res: T): ResolveRes[T] = ResLogged(Some(res), Nil)

  def fromLogs[T](logs: Seq[CompilerMsg]): ResolveRes[T] = ResLogged(None, logs)

  def empty[T]: ResolveRes[T] = ResLogged(None, Nil)

  def fromBoth[T](res: T, logs: Seq[CompilerMsg]): ResolveRes[T] = ResLogged(res, logs)

  def unapply(res: ResolveRes[T]): (Option[T], Seq[CompilerMsg]) = (res.value, res.logs)
}

extension [T](res: ResolveRes[T]) {
  def asResLogged: ResLogged[Option[T]] = this

  def value: Option[T] = res.value

  def logs: Seq[CompilerMsg] = res.logs

  def isEmpty: Boolean = res.value.isEmpty

  def getOrElse(orElse: => T): ResLogged[T] = ResLogged(res.value.getOrElse(orElse), res.logs)

  def orElse(orElse: => Option[T]): ResolveRes[T] = ResLogged(res.value.orElse(orElse), res.logs)

  def map[R](fn: T => R): ResolveRes[R] = {
    res.value match {
      case Some(value) => ResLogged(value, res.logs)
      case None => res
    }
  }

  def filter(fn: T => Boolean): ResolveRes[T] = res.value match {
    case Some(value) if fn(value) => res
    case _ => ResolveRes.empty
  }

  def fold[R](orElse: => R)(fn: T => R): ResLogged[R] = ResLogged(res.value.fold(orElse)(fn), res.logs)

  def flatMap[R](fn: T => Option[R]): ResolveRes[R] = {
    val newVal = res.value.flatMap(fn)
    ResLogged(newVal, res.logs)
  }

  def flatMapR[R](fn: T => ResolveRes[R]): ResolveRes[R] = {
    res.value match {
      case Some(value) =>
        val ResLogged(value2, logs2) = fn(value)
        ResLogged(value2, res.logs ++ logs2)
      case None => res
    }
  }

  def mapLogs(fn: Seq[CompilerMsg] => Seq[CompilerMsg]): ResLogged[T] = {
    ResLogged(res.value, fn(logs))
  }
}

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

/**
 * Log a compiler message given an implicit Messages object. This returns false
 * to make it easier to return false from inside methods that return booleans
 * representing whether or not an operation was successful.
 * TODO Return Unit instead for the sake of good practice?
 */ 
def sendMsg(msg: CompilerMsg)(using msgs: Messages): false = {
  msgs.msgs += msg
  false
}
