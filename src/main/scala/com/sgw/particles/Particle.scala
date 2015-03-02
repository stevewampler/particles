package com.sgw.particles

import com.sgw.particles.utils.JSON

object Particle {
  def apply(config: JSON): Particle = {
    Particle(
      m = config.getDouble("mass").getOrElse(0.0),
      p = config.getVector3D("pos").getOrElse(Vector3D.ZeroValue),
      v = config.getVector3D("val").getOrElse(Vector3D.ZeroValue),
      a = config.getVector3D("acc").getOrElse(Vector3D.ZeroValue),
      t = config.getDouble("time").getOrElse(0),
      radius = config.getDouble("radius").getOrElse(0),
      age = config.getDouble("age").getOrElse(0),
      area = config.getDouble("area").getOrElse(1)
    )
  }
}

case class Particle(
    var m: Double = 1.0,
    var p: Vector3D = Vector3D(),
    var v: Vector3D = Vector3D(),
    var a: Vector3D = Vector3D(),
    var t: Double = 0.0,
    var radius: Double = 1.0,
    var age: Double = 0.0,
    var area: Double = 1.0) {
  var pLast = Vector3D()
  var vLast = Vector3D()
  var aLast = Vector3D()
  var tLast = 0.0

  def next = {
    pLast = p
    vLast = v
    aLast = a
    tLast = t
    a = Vector3D.ZeroValue
  }

  def apply(t: Double) = {
    val dt = t - this.t

    if (m != Double.MaxValue) {
      v = v + (aLast + a) / 2.0 * dt
      p = p + (vLast + v) / 2.0 * dt
    }

    this.t = t

    age = age + dt
  }

  def speed = v.len
}


