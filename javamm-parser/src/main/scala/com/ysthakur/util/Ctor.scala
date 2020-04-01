package com.ysthakur.util

/**
 * A constructor
 * @tparam I
 * @tparam T
 */
trait Ctor[I, T] {
  type This = T
  type In = I
  def create(input: In): This
}