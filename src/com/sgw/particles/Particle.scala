package com.sgw.particles

trait CanDie {
  def dead: Boolean
}

case class Particle(
    var m: Double = 1.0,
    var p: Vector3D = Vector3D(),
    var v: Vector3D = Vector3D(),
    var a: Vector3D = Vector3D(),
    var t: Double = 0.0,
    var radius: Double = 1.0,
    var age: Double = 0.0,
    var area: Double = 1.0,
    val maxAge: Double = Double.MaxValue) extends CanDie with Updatable {
  var pLast = Vector3D()
  var vLast = Vector3D()
  var aLast = Vector3D()
  var tLast = 0.0

  override def beginUpdate(toTime: Double) = {
    pLast = p
    vLast = v
    aLast = a
    tLast = t
    a = Vector3D.ZeroValue
  }

  def update(toTime: Double) = {
    val dt = toTime - this.t

    if (m != Double.MaxValue) {
      v = v + (aLast + a) / 2.0 * dt
      p = p + (vLast + v) / 2.0 * dt
    }

    this.t = toTime

    age = age + dt
  }

  def speed = v.len

  def dead = age > maxAge
}


