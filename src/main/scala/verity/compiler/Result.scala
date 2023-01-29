package verity.compiler

import verity.compiler.ast.Span

import java.io.File
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

import cats.data.{Chain, Writer}
import cats.kernel.Semigroup
import cats.syntax.all.*
import cats.FlatMap
import cats.Foldable
import cats.Monad

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

case class Result[T](value: T, msgs: Chain[Message])

object Result {

  /** A successful result with no messages */
  def from[T](t: T): Result[T] = Result(t, Chain.nil)

  def some[T](t: T, errors: Chain[Message] = Chain.nil): Result[Option[T]] =
    Result(Some(t), errors)

  def none[T](errors: Chain[Message] = Chain.nil): Result[Option[T]] =
    Result(None, errors)

  /** Just a message for an optional result */
  def msg[T](msg: Message): Result[Option[T]] = Result(None, Chain(msg))

  /** Check if any of the messages in the result are errors */
  def hasError[T](res: Result[T]): Boolean =
    res.msgs.exists(_.level == Message.LogLevel.Error)

  /** Combine a bunch of results.
    *
    * If combining a list of optional results, see [[combineAllOpts]]
    * @return
    *   A single result with a list of all the results' values. All the messages
    *   will be concatenated together.
    */
  def combineAll[F[_], T](
      results: F[Result[T]]
  )(using Foldable[F]): Result[Chain[T]] =
    results.foldLeft(Result(Chain.nil, Chain.nil)) { (acc, next) =>
      Result(acc.value :+ next.value, acc.msgs ++ next.msgs)
    }

  /** Combine a bunch of optional results.
    *
    * @return
    *   A single result with a list of all the values from the results that
    *   weren't empty. All the messages will be concatenated together, including
    *   the results that returned Nones.
    */
  def combineAllOpts[F[_], T](
      results: F[Result[Option[T]]]
  )(using Foldable[F]): Result[Chain[T]] =
    results.foldLeft(Result(Chain.nil, Chain.nil)) { (acc, next) =>
      Result(
        next.value.fold(acc.value)(acc.value.append),
        acc.msgs ++ next.msgs
      )
    }

  given [T](using Semigroup[T]): Semigroup[Result[T]] with
    def combine(first: Result[T], second: Result[T]): Result[T] =
      Result(
        Semigroup.combine(first.value, second.value),
        Semigroup.combine(first.msgs, second.msgs)
      )

  given FlatMap[Result] with
    def map[A, B](res: Result[A])(f: A => B): Result[B] =
      Result(f(res.value), res.msgs)

    def flatMap[A, B](res: Result[A])(f: A => Result[B]): Result[B] =
      val newRes = f(res.value)
      Result(newRes.value, res.msgs ++ newRes.msgs)

    def tailRecM[A, B](init: A)(f: A => Result[Either[A, B]]): Result[B] = {
      @tailrec
      def tailRecImpl(init: A, msgs: Chain[Message]): Result[B] = {
        val Result(value, newMsgs) = f(init)
        value match {
          case Left(err)  => tailRecImpl(err, msgs ++ newMsgs)
          case Right(res) => Result(res, msgs ++ newMsgs)
        }
      }
      tailRecImpl(init, Chain.nil)
    }
}
