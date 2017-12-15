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
  m: Option[Double],
  p: Option[Vector3D],
  v: Option[Vector3D],
  a: Option[Vector3D],
  f: Option[Vector3D],
  t: Option[Double],
  radius: Option[Double],
  age: Option[Double],
  area: Option[Double]
) extends ParticleFactory {
  def createParticles: List[Particle] = List(
    Particle(
      id = id,
      name = name.getOrElse(id.toString),
      t = t.getOrElse(0.0),
      m = m.getOrElse(1.0),
      p = p.getOrElse(Vector3D.ZeroValue),
      v = v.getOrElse(Vector3D.ZeroValue),
      a = a.getOrElse(Vector3D.ZeroValue),
      f = f.getOrElse(Vector3D.ZeroValue),
      m1 = m.getOrElse(1.0),
      f1 = f.getOrElse(Vector3D.ZeroValue),
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
  t: Double,
  m: Double,
  p: Vector3D,
  v: Vector3D,
  a: Vector3D,
  f: Vector3D,
  m1: Double,
  f1: Vector3D,
  radius: Double,
  age: Double,
  area: Double
) {
  /**
    * Applies the specified time delta to this particle, currently at time t0, and returns a new particle system
    * containing the new particle at time t1.
    *
    * @param pSys the particle system to which this particle belongs
    * @param dt the time delta
    *
    * @return a new ParticleSystem containing a copy of this particle at time t1
    */
  def apply(pSys: ParticleSystem)(dt: Double): ParticleSystem = {
    val t1 = t + dt

    val a1 = if (m != Double.MaxValue) {
      (f + f1) / 2.0 / (m + m1) / 2.0
//      f1 / m1
    } else {
      a
    }

    val v1 = v + (a + a1) / 2.0 * dt
    val p1 = p + (v + v1) / 2.0 * dt

    pSys.copy(
      particleMap = pSys.particleMap.updated(
        id,
        copy(
          t = t1,
          m = m1,
          p = p1,
          v = v1,
          a = a1,
          f = f1,
          f1 = Vector3D.ZeroValue,
          age = age + dt
        )
      )
    )
  }

  def speed: Double = v.len

  def distance(p2: Particle): Double = (p2.p - p).len
}


