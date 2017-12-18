package com.sgw.particles.model

import play.api.libs.json._

object ParticleFunction1 {
  implicit def playFormat: Format[ParticleFunction1] = new Format[ParticleFunction1] {

    override def reads(json: JsValue): JsResult[ParticleFunction1] =
      (json \ "type")
        .validate[String]
        .flatMap {
          case "ConstantVector3DParticleFunction" => ConstantVector3DParticleFunction1.playFormat.reads(json)
          case "ParticleTimeFunction" => ParticleTimeFunction.playFormat.reads(json)
          case "ParticlePositionFunction" => ParticlePositionFunction.playFormat.reads(json)
          case "ParticlePositionAndTimeFunction" => ParticlePositionAndTimeFunction.playFormat.reads(json)
          case "ParticleVelocityFunction" => ParticleVelocityFunction.playFormat.reads(json)
          case "CompositeParticleFunction1" => CompositeParticleFunction1.playFormat.reads(json)
        }

    override def writes(func: ParticleFunction1): JsValue = {
      func match {
        case func: ConstantVector3DParticleFunction1 => ConstantVector3DParticleFunction1.playFormat.writes(func)
        case func: ParticleTimeFunction => ParticleTimeFunction.playFormat.writes(func)
        case func: ParticlePositionFunction => ParticlePositionFunction.playFormat.writes(func)
        case func: ParticlePositionAndTimeFunction => ParticlePositionAndTimeFunction.playFormat.writes(func)
        case func: ParticleVelocityFunction => ParticleVelocityFunction.playFormat.writes(func)
        case func: ParticleAccelerationFunction => ParticleAccelerationFunction.playFormat.writes(func)
        case func: CompositeParticleFunction1 => CompositeParticleFunction1.playFormat.writes(func)
      }
    }
  }
}

/**
  * A function that, when applied to a particle, returns a Vector3D.
  */
trait ParticleFunction1 {
  def apply(p: Particle): Vector3D
}

/**
  * A function that, when applied to two particles, returns a Vector3D.
  */
trait ParticleFunction2 {
  def apply(p1: Particle, p2: Particle): Vector3D
}

/*
case class SinWaveParticleFunction(
    offset: Vector3D = Vector3D(10.0), // m/s
    amplitude: Vector3D = Vector3D(50.0), // m/s
    frequency: Vector3D = Vector3D(0.25, 1.0, 1.0), // cycles/sec
    phase: Vector3D = Vector3D()
    // posPeriod: Vector3D = Vector3D(200.0, 0.0, 0.0), // m
    // timePeriod: Double = 4.0 // s
) extends ParticleFunction {
  /*
  val posMult = Vector3D(
    if (posPeriod.x != 0.0) (2.0 * Math.PI) / posPeriod.x else 0.0,
    if (posPeriod.y != 0.0) (2.0 * Math.PI) / posPeriod.y else 0.0,
    if (posPeriod.z != 0.0) (2.0 * Math.PI) / posPeriod.z else 0.0
  )
  */
  // val timeMult = 2.0 * Math.PI / timePeriod
  def apply(p: Particle) = offset + (amplitude * (frequency * 2 * Math.PI * p.t + phase).sin)
}
*/

object ConstantVector3DParticleFunction1 {
  implicit val playFormat: Format[ConstantVector3DParticleFunction1] = new Format[ConstantVector3DParticleFunction1] {
    override def reads(json: JsValue): JsResult[ConstantVector3DParticleFunction1] =
      Json.reads[ConstantVector3DParticleFunction1].reads(json)

    override def writes(func: ConstantVector3DParticleFunction1): JsValue =
      Json.writes[ConstantVector3DParticleFunction1].writes(func).deepMerge(
        Json.obj("type" -> JsString(func.getClass.getName))
      )
  }
}

/**
  * A ParticleFunction that always returns the specified Vector3D.
  */
case class ConstantVector3DParticleFunction1(vector: Vector3D) extends ParticleFunction1 {
  def apply(p: Particle) = vector
}

object ParticleTimeFunction {
  implicit val playFormat: Format[ParticleTimeFunction] = new Format[ParticleTimeFunction] {
    override def reads(json: JsValue) = Json.reads[ParticleTimeFunction].reads(json)

    override def writes(func: ParticleTimeFunction): JsValue = Json.writes[ParticleTimeFunction].writes(func).deepMerge(
      Json.obj("type" -> JsString(func.getClass.getName))
    )
  }
}

/**
  * A ParticleFunction that applies the specified function to a particle's time.
  * @param func The function to be applied to a Particle's time.
  */
case class ParticleTimeFunction(func: Vector3DFunction) extends ParticleFunction1 {
  def apply(p: Particle) = func(Vector3D(p.t, p.t, p.t))
}

object ParticlePositionFunction {
  implicit val playFormat: Format[ParticlePositionFunction] = new Format[ParticlePositionFunction] {
    override def reads(json: JsValue) = Json.reads[ParticlePositionFunction].reads(json)

    override def writes(func: ParticlePositionFunction): JsValue = Json.writes[ParticlePositionFunction].writes(func).deepMerge(
      Json.obj("type" -> JsString(func.getClass.getName))
    )
  }
}

/**
  * A ParticleFunction that applies a specified Vector3DFunction to a Particle's position.
  * @param func The function to be applied to a Particle's position.
  */
case class ParticlePositionFunction(func: Vector3DFunction) extends ParticleFunction1 {
  def apply(p: Particle) = func(p.p)
}

object ParticlePositionAndTimeFunction {
  implicit val playFormat: Format[ParticlePositionAndTimeFunction] = new Format[ParticlePositionAndTimeFunction] {
    override def reads(json: JsValue) = Json.reads[ParticlePositionAndTimeFunction].reads(json)

    override def writes(func: ParticlePositionAndTimeFunction): JsValue = Json.writes[ParticlePositionAndTimeFunction].writes(func).deepMerge(
      Json.obj("type" -> JsString(func.getClass.getName))
    )
  }
}

/**
  * A ParticleFunction that applies a specified Vector3DFunction to a Particle's position and time.
  * @param func The function to be applied to a Particle's position.
  */
case class ParticlePositionAndTimeFunction(func: Vector3DFunction) extends ParticleFunction1 {
  def apply(p: Particle) = func(p.p + p.t)
}

object ParticleVelocityFunction {
  implicit val playFormat: Format[ParticleVelocityFunction] = new Format[ParticleVelocityFunction] {
    override def reads(json: JsValue) = Json.reads[ParticleVelocityFunction].reads(json)

    override def writes(func: ParticleVelocityFunction): JsValue = Json.writes[ParticleVelocityFunction].writes(func).deepMerge(
      Json.obj("type" -> JsString(func.getClass.getName))
    )
  }
}

/**
  * A ParticleFunction that applies a specified Vector3DFunction to a Particle's velocity.
  * @param func The function to be applied to a Particle's velocity.
  */
case class ParticleVelocityFunction(func: Vector3DFunction) extends ParticleFunction1 {
  def apply(p: Particle) = func(p.v)
}

object ParticleAccelerationFunction {
  implicit val playFormat: Format[ParticleAccelerationFunction] = new Format[ParticleAccelerationFunction] {
    override def reads(json: JsValue) = Json.reads[ParticleAccelerationFunction].reads(json)

    override def writes(func: ParticleAccelerationFunction): JsValue = Json.writes[ParticleAccelerationFunction].writes(func).deepMerge(
      Json.obj("type" -> JsString(func.getClass.getName))
    )
  }
}

/**
  * A ParticleFunction that applies a specified Vector3DFunction to a Particle's acceleration.
  * @param func The function to be applied to a Particle's acceleration.
  */
case class ParticleAccelerationFunction(func: Vector3DFunction) extends ParticleFunction1 {
  def apply(p: Particle) = func(p.a)
}

object CompositeParticleFunction1 {
  implicit val playFormat: Format[CompositeParticleFunction1] = new Format[CompositeParticleFunction1] {
    override def reads(json: JsValue) = Json.reads[CompositeParticleFunction1].reads(json)

    override def writes(func: CompositeParticleFunction1): JsValue = Json.writes[CompositeParticleFunction1].writes(func).deepMerge(
      Json.obj("type" -> JsString(func.getClass.getName))
    )
  }
}

case class CompositeParticleFunction1(funcs: Seq[ParticleFunction1]) extends ParticleFunction1 {
  def apply(p: Particle) = funcs.foldLeft(Vector3D())((z, func) => z + func(p))
}
