package com.sgw.particles.model

import play.api.libs.json._

object ParticleFunction {
  implicit def playFormat: Format[ParticleFunction] = new Format[ParticleFunction] {

    override def reads(json: JsValue): JsResult[ParticleFunction] =
      (json \ "type")
        .validate[String]
        .flatMap {
          case "ConstantVector3DParticleFunction" => ConstantVector3DParticleFunction.playFormat.reads(json)
          case "ParticleTimeFunction" => ParticleTimeFunction.playFormat.reads(json)
          case "ParticlePositionFunction" => ParticlePositionFunction.playFormat.reads(json)
          case "ParticleVelocityFunction" => ParticleVelocityFunction.playFormat.reads(json)
          case "CompositeParticleFunction" => CompositeParticleFunction.playFormat.reads(json)
        }

    override def writes(func: ParticleFunction): JsValue = {
      func match {
        case func: ConstantVector3DParticleFunction => ConstantVector3DParticleFunction.playFormat.writes(func)
        case func: ParticleTimeFunction => ParticleTimeFunction.playFormat.writes(func)
        case func: ParticlePositionFunction => ParticlePositionFunction.playFormat.writes(func)
        case func: ParticleVelocityFunction => ParticleVelocityFunction.playFormat.writes(func)
        case func: ParticleAccelerationFunction => ParticleAccelerationFunction.playFormat.writes(func)
        case func: CompositeParticleFunction => CompositeParticleFunction.playFormat.writes(func)
      }
    }
  }
}

/**
  * A function that, when applied to a particle, returns a Vector3D.
  */
trait ParticleFunction {
  def apply(p: Particle): Vector3D
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

object ConstantVector3DParticleFunction {
  implicit val playFormat: Format[ConstantVector3DParticleFunction] = new Format[ConstantVector3DParticleFunction] {
    override def reads(json: JsValue): JsResult[ConstantVector3DParticleFunction] =
      Json.reads[ConstantVector3DParticleFunction].reads(json)

    override def writes(func: ConstantVector3DParticleFunction): JsValue =
      Json.writes[ConstantVector3DParticleFunction].writes(func).deepMerge(
        Json.obj("type" -> JsString(func.getClass.getName))
      )
  }
}

/**
  * A ParticleFunction that always returns the specified Vector3D.
  */
case class ConstantVector3DParticleFunction(vector: Vector3D) extends ParticleFunction {
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
case class ParticleTimeFunction(func: Vector3DFunction) extends ParticleFunction {
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
case class ParticlePositionFunction(func: Vector3DFunction) extends ParticleFunction {
  def apply(p: Particle) = func(p.p)
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
case class ParticleVelocityFunction(func: Vector3DFunction) extends ParticleFunction {
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
case class ParticleAccelerationFunction(func: Vector3DFunction) extends ParticleFunction {
  def apply(p: Particle) = func(p.a)
}

object CompositeParticleFunction {
  implicit val playFormat: Format[CompositeParticleFunction] = new Format[CompositeParticleFunction] {
    override def reads(json: JsValue) = Json.reads[CompositeParticleFunction].reads(json)

    override def writes(func: CompositeParticleFunction): JsValue = Json.writes[CompositeParticleFunction].writes(func).deepMerge(
      Json.obj("type" -> JsString(func.getClass.getName))
    )
  }
}

case class CompositeParticleFunction(funcs: Seq[ParticleFunction]) extends ParticleFunction {
  def apply(p: Particle) = funcs.foldLeft(Vector3D())((z, func) => z + func(p))
}
