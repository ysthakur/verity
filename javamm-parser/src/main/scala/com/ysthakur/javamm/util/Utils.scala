package com.ysthakur.javamm.util

def[T] (option: Option[T]) `?:` (orElse: T): T =
  option match {
    case Some (v) => v
    case None => orElse
  }