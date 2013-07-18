package com.sgw.particles

/**
 * author: steve
 */
case class Bounds3D(min: Vector3D, max: Vector3D) {
  def width  = max.x - min.x
  def height = max.y - min.y
  def depth  = max.z - min.z
  def size = Vector3D(width, height, depth)
  def maxBound = width.max(height).max(depth)

  def scale(factor: Double) = Bounds3D(min * factor, max * factor)

  def *(factor: Double) = scale(factor)
}
