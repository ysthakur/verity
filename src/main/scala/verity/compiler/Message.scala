package verity.compiler

import verity.compiler.ast.Span

import java.io.File
import cats.data.{Chain, Writer}

/** A message from the compiler
  *
  * @param msg
  *   The contents of the message
  * @param span
  *   The location of the code that the message refers to
  * @param filename
  *   The file in which the code is. TODO make this an Option[File]?
  * @param level
  *   The severity of the message
  */
case class Message(
  msg: String,
  span: Span,
  filename: String,
  level: Message.LogLevel
)

object Message {

  /** The severity of a message. */
  enum LogLevel {
    case Debug, Info, Warn, Error
  }

  def err(msg: String, span: Span, filename: String) =
    Message(msg, span, filename, LogLevel.Error)
  def warn(msg: String, span: Span, filename: String) =
    Message(msg, span, filename, LogLevel.Warn)
  def info(msg: String, span: Span, filename: String) =
    Message(msg, span, filename, LogLevel.Info)
  def debug(msg: String, span: Span, filename: String) =
    Message(msg, span, filename, LogLevel.Debug)
}

type Result[T] = Writer[Chain[Message], T]

object Result {
  /** A successful result with no messages */
  def from[T](t: T): Result[T] = Writer(Chain.nil, t)

  def some[T](t: T, errors: Chain[Message] = Chain.nil): Result[Option[T]] =
    Writer(errors, Some(t))

  def none[T](errors: Chain[Message] = Chain.nil): Result[Option[T]] =
    Writer(errors, None)

  /** Just a message for an optional result */
  def msg[T](msg: Message): Result[Option[T]] = Writer(Chain(msg), None)

  /** Check if any of the messages in the result are errors */
  def hasError[T](res: Result[T]): Boolean =
    res.written.exists(_.level == Message.LogLevel.Error)
}
