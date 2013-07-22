package com.sgw.particles

trait Vector3DFunction {
  def apply(v: Vector3D): Vector3D
}

case class ConstantVector3DFunction(const: Vector3D) extends Vector3DFunction {
  def apply(v: Vector3D) = const
}

/**
 * A Vector3DFunction that returns a random Vector3D between the specified min and max Vector3Ds.
 * @param min The minimum value of the resulting random Vector.
 * @param max The maximum value of hte resulting random Vector.
 */
case class RandomVector3DFunction(min: Vector3D, max: Vector3D) extends Vector3DFunction {
  def apply(v: Vector3D) = Vector3D(
    Math.random() * (max.x - min.x) + min.x,
    Math.random() * (max.y - min.y) + min.y,
    Math.random() * (max.z - min.z) + min.z
  )
}

case class SinWaveVector3DFunction(
  offset: Vector3D    = Vector3D(10.0), // m/s
  amplitude: Vector3D = Vector3D(50.0), // m/s
  frequency: Vector3D = Vector3D(0.25, 1.0, 1.0), // cycles/sec
  phase: Vector3D     = Vector3D() // radians
) extends Vector3DFunction {
  def apply(theta: Vector3D) = offset + (amplitude * (theta * frequency * 2 * Math.PI + phase).sin)
}

object XYNormalVector3DFunction extends Vector3DFunction {
  def apply(v: Vector3D) = v.xyNormal
}

object NormalizeVector3DFunction extends Vector3DFunction {
  def apply(v: Vector3D) = v.normalize
}

case class TranslateVector3D(t: Vector3D) extends Vector3DFunction {
  def apply(v: Vector3D) = v + t
}

case class ScaleVector3DFunction(s: Double) extends Vector3DFunction {
  def apply(v: Vector3D) = v * s
}

case class LinearScaleVector3DFunction(m: Double, b: Double) extends Vector3DFunction {
  def apply(v: Vector3D) = v * m + v.normalize * b
}

case class CompositeVector3DFunction(funcs: Seq[Vector3DFunction]) extends Vector3DFunction {
  def apply(v: Vector3D) = funcs.foldLeft(v)((v, func) => func(v))
}
