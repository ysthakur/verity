package com.ysthakur.util

import scala.reflect.ClassTag

// type |[A, B]  = Either[A, B]
// type ->[A, B] = (A, B)

// /*implicit*/ def toLeft[A, B](a: A): A Either B = Left[A, B](a)

// /*implicit*/ def toRight[A, B](b: B): A Either B = Right[A, B](b)

def[T] (option: Option[T]) `?:` (orElse: T): T =
  option match {
    case Some (v) => v
    case None => orElse
  }

trait Utils {
  def (t: Any).as: Any = t.asInstanceOf[Any]
}

given utils as Utils

// implicit class ElvisOperator[T](obj: T) {
//   def ?:(orElse: T): T = if (obj == null) orElse else obj
// }

def[T: ClassTag] (t: Any).as: T = t.asInstanceOf[T]

/*
  implicit class InstanceOf(t: Any) {
      def is[T : ClassTag]: Boolean = t.isInstanceOf[T]
  }*/
