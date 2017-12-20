package com.sgw.particles.utils

object Utils {

  def maybe[T](condition: => Boolean)(ifTrue: => T): Option[T] = if (condition) {
    Some(ifTrue)
  } else {
    None
  }

  def time[A, B](label: String)(f: => B): B = {
    val start = System.currentTimeMillis()
    val result = f
    val end = System.currentTimeMillis()
    println(s"$label: ${end-start} ms")
    result
  }
}
