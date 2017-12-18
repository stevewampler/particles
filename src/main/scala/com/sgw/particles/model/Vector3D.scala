package com.sgw.particles.model

import play.api.libs.json.{Format, Json}

object Vector3D {
  implicit val playFormat: Format[Vector3D] = Json.format[Vector3D]

  lazy val ZeroValue = Vector3D()
  lazy val MaxValue  = Vector3D(Double.MaxValue, Double.MaxValue, Double.MaxValue)
  lazy val MinValue  = Vector3D(Double.MinValue, Double.MinValue, Double.MinValue)
}

case class Vector3D(x: Double = 0.0, y: Double = 0.0, z: Double = 0.0) {

  def len = Math.sqrt(x*x + y*y + z*z)

  def sub(v2: Vector3D) = Vector3D(x - v2.x, y - v2.y, z - v2.z)
  def add(v2: Vector3D) = Vector3D(x + v2.x, y + v2.y, z + v2.z)
  def add(s: Double) = Vector3D(x + s, y + s, z + s)
  def cross_product(v2: Vector3D) = Vector3D(x * v2.x, y * v2.y, z * v2.z)
  def dotProduct(v2: Vector3D) = x * v2.x + y * v2.y + z * v2.z
  def squared = cross_product(this)
  def inverse = Vector3D(1.0/x, 1.0/y, 1.0/z)
  def bounds(v2: Vector3D): Pair[Vector3D, Vector3D] = Pair(min(v2), max(v2))
  def max(v2: Vector3D) = Vector3D(x.max(v2.x), y.max(v2.y), z.max(v2.z))
  def min(v2: Vector3D) = Vector3D(x.min(v2.x), y.min(v2.y), z.min(v2.z))
  def sin  = Vector3D(Math.sin(x), Math.sin(y), Math.sin(z))

  def scale(s: Double)  = Vector3D(x * s, y * s, z * s)
  def normalize = {
    val vlen = len
    if (vlen < 0.000001) Vector3D.ZeroValue else this / len
  }

  def round: Vector3D = Vector3D(x.round, y.round, z.round)

  def roundTo(decimalPlaces: Int): Vector3D = {
    val s = Math.pow(10.0, decimalPlaces)

    scale(s).round.scale(1.0/s)
  }

  def scalarProjectionOnTo(v2: Vector3D): Double = dotProduct(v2.normalize)

  def vectorProjectionOnTo(v2: Vector3D): Vector3D = {
    val v2norm = v2.normalize
    v2norm * this.dotProduct(v2norm)
  }

  def -(v2: Vector3D) = sub(v2)
  def +(v2: Vector3D) = add(v2)
  def +(s: Double)    = add(s)
  def *(v2: Vector3D) = cross_product(v2)
  def /(v2: Vector3D) = cross_product(v2.inverse)
  def *(s: Double)    = scale(s)
  def /(s: Double)    = scale(1.0/s)

  override def toString: String = s"($x, $y, $z)"

  def toString(decimals: Int): String = {
    val formatter = s"%1.${decimals}f"

    List(
      formatter.format(x),
      formatter.format(y),
      formatter.format(z)
    ).mkString("(", ",", ")")
  }
}

