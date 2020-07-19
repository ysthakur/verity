package com.ysthakur.verity

class Lazy[T](body: => T) {
  lazy val get = body
}
object Lazy {
  def apply[T](body: => T): Lazy[T] = new Lazy(body) 
}