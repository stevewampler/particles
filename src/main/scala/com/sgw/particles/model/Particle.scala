package com.sgw.particles.model

import play.api.libs.json._

object ParticleFactory {
  implicit def playFormat: Format[ParticleFactory] = new Format[ParticleFactory] {

    override def reads(json: JsValue): JsResult[ParticleFactory] =
      (json \ "type")
        .validate[String]
        .flatMap {
          case "Simple" => SimpleParticleFactory.playFormat.reads(json)
        }

    override def writes(func: ParticleFactory): JsValue = {
      func match {
        case factory: SimpleParticleFactory => SimpleParticleFactory.playFormat.writes(factory)
      }
    }
  }
}

sealed trait ParticleFactory {
  def createParticles: List[Particle]
}

object SimpleParticleFactory {
  implicit val playFormat: Format[SimpleParticleFactory] = new Format[SimpleParticleFactory] {
    override def reads(json: JsValue) = Json.reads[SimpleParticleFactory].reads(json)

    override def writes(f: SimpleParticleFactory): JsValue = Json.writes[SimpleParticleFactory].writes(f).deepMerge(
      Json.obj("type" -> JsString("Simple"))
    )
  }
}

case class SimpleParticleFactory(
  id: Particle.ID,
  name: Option[String],
  var m: Option[Double],
  var p: Option[Vector3D],
  var v: Option[Vector3D],
  var a: Option[Vector3D],
  var t: Option[Double],
  var radius: Option[Double],
  var age: Option[Double],
  var area: Option[Double]
) extends ParticleFactory {
  def createParticles: List[Particle] = List(
    Particle(
      id = id,
      name = name.getOrElse(id.toString),
      m = m.getOrElse(1.0),
      p = p.getOrElse(Vector3D()),
      v = v.getOrElse(Vector3D()),
      a = a.getOrElse(Vector3D()),
      t = t.getOrElse(0.0),
      radius = radius.getOrElse(1.0),
      age = age.getOrElse(0.0),
      area = area.getOrElse(1.0)
    )
  )
}

object Particle {
  type ID = Long
}

case class Particle(
  id: Particle.ID,
  name: String,
  var m: Double,
  var p: Vector3D,
  var v: Vector3D,
  var a: Vector3D,
  var t: Double,
  var radius: Double,
  var age: Double,
  var area: Double
) {
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

  def distance(p2: Particle): Double = (p2.p - p).len
}


