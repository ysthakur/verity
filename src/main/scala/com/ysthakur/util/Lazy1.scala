package com.ysthakur.util

class Lazy1[I, O](private val f: I => O) {
  private var field: Any = _
  private var initialized = false
  def get(i: I): O = {
    if (!initialized) {
      field = f(i)
      initialized = true
    }
    field.asInstanceOf[O]
  }
}
