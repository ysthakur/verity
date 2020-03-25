package com.ysthakur

package object util {

    type |[A, B] = Either[A, B]

    val x: Int|String = 1

    implicit def toLeft[A, B](a: A): A Either B = Left[A, B](a)
    implicit def toRight[A, B](b: B): A Either B = Right[A, B](b)

    implicit class ElvisOpplicable[T](option: Option[T]) {
        def ?:(orElse: T): T =
            option match {
                case Some(v) => v
                case None => orElse
            }
    }
    implicit class ElvisOperator[T](obj: T) {
        def ?:(orElse: T): T = if (obj == null) orElse else obj
    }

    def cast[T]: T = asInstanceOf[T]
}
