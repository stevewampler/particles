package com.sgw.particles

/**
 * Created with IntelliJ IDEA.
 * User: swampler
 * Date: 4/4/13
 * Time: 8:03 PM
 * Copyright 2013: Steve Wampler
 */
object Vector3D {
  def apply(x: Double = 0.0, y: Double = 0.0, z: Double = 0.0) = new Vector3D(x, y, z)

  lazy val ZeroValue = Vector3D()
  lazy val MaxValue  = Vector3D(Double.MaxValue, Double.MaxValue, Double.MaxValue)
  lazy val MinValue  = Vector3D(Double.MinValue, Double.MinValue, Double.MinValue)
}

class Vector3D(val x: Double = 0.0, val y: Double = 0.0, val z: Double = 0.0) extends IndexedSeq[Double] {
  val length = 3

  def apply(i: Int) = i match {
    case 0 => x
    case 1 => y
    case 2 => z
    case _ => throw new ArrayIndexOutOfBoundsException(i)
  }

  def len = Math.sqrt(x*x + y*y + z*z)

  def sub(v2: Vector3D) = Vector3D(x - v2.x, y - v2.y, z - v2.z)
  def add(v2: Vector3D) = Vector3D(x + v2.x, y + v2.y, z + v2.z)
  def add(s: Double) = Vector3D(x + s, y + s, z + s)
  def cross_product(v2: Vector3D) = Vector3D(x * v2.x, y * v2.y, z * v2.z)
  def dot_product(v2: Vector3D) = x * v2.x + y * v2.y + z * v2.z
  def squared = cross_product(this)
  def inverse = Vector3D(1.0/x, 1.0/y, 1.0/z)
  def bounds(v2: Vector3D): Pair[Vector3D, Vector3D] = Pair(min(v2), max(v2))
  def max(v2: Vector3D) = Vector3D(x.max(v2.x), y.max(v2.y), z.max(v2.z))
  def min(v2: Vector3D) = Vector3D(x.min(v2.x), y.min(v2.y), z.min(v2.z))
  def sin  = Vector3D(Math.sin(x), Math.sin(y), Math.sin(z))

  def scale(s: Double)  = Vector3D(x * s, y * s, z * s)
  def normalize = {
    val vlen = len
    if (vlen < 0.000001) Vector3D() else this / len
  }
  def xyNormal = Vector3D(-y, x, z)

  def projectOnTo(v2: Vector3D) = {
    val v2norm = v2.normalize
    v2norm * dot_product(v2norm)
  }

  def -(v2: Vector3D) = sub(v2)
  def +(v2: Vector3D) = add(v2)
  def +(s: Double)    = add(s)
  def *(v2: Vector3D) = cross_product(v2)
  def /(v2: Vector3D) = cross_product(v2.inverse)
  def *(s: Double)    = scale(s)
  def /(s: Double)    = scale(1.0/s)
}

