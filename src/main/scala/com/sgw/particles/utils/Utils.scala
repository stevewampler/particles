package com.sgw.particles.utils

object Utils {

  def maybe[T](condition: => Boolean)(ifTrue: => T): Option[T] = if (condition) {
    Some(ifTrue)
  } else {
    None
  }
}
