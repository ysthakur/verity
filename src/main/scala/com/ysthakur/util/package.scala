package com.ysthakur

package object util {

    type |[A, B] = Either[A, B]

    implicit def toEither[A, B](a: A): Either[A, B] = Left(a)
    implicit def toEither[A, B](b: B): Either[A, B] = Right(b)

    implicit class ElvisOpplicable[T](option: Option[T]) {
        def ?:(orElse: T): T =
            option match {
                case Some(v) => v
                case None => orElse
            }
    }

    def cast[T]: T = asInstanceOf[T]
}
