package com.ysthakur

import scala.reflect.ClassTag

package object util {

  type |[A, B]  = Either[A, B]
  type ->[A, B] = (A, B)

  implicit def toLeft[A, B](a: A): A Either B = Left[A, B](a)

  implicit def toRight[A, B](b: B): A Either B = Right[A, B](b)

  implicit class ElvisOpplicable[T](option: Option[T]) {
    def ?:(orElse: T): T =
      option match {
        case Some(v) => v
        case None    => orElse
      }
  }

  implicit class ElvisOperator[T](obj: T) {
    def ?:(orElse: T): T = if (obj == null) orElse else obj
  }

  implicit class Castable(t: Any) {
    def as[T: ClassTag]: T = t.asInstanceOf[T]
  }
  /*
    implicit class InstanceOf(t: Any) {
        def is[T : ClassTag]: Boolean = t.isInstanceOf[T]
    }*/

}
