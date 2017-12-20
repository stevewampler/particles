package com.sgw.particles.model

import play.api.libs.json._

object ParticleFunction1 {
  implicit def playFormat: Format[ParticleFunction1] = new Format[ParticleFunction1] {

    override def reads(json: JsValue): JsResult[ParticleFunction1] =
      (json \ "type")
        .validate[String]
        .flatMap {
          case "ConstantParticleFunction" => ConstantParticleFunction.playFormat.reads(json)
          case "ParticleTimeFunction" => ParticleTimeFunction.playFormat.reads(json)
          case "ParticlePositionFunction" => ParticlePositionFunction.playFormat.reads(json)
          case "ParticleVelocityFunction" => JsSuccess(ParticleVelocityFunction)
          case "ParticleAccelerationFunction" => JsSuccess(ParticleAccelerationFunction)
          case "CompositeParticleFunction" => CompositeParticleFunction.playFormat.reads(json)
          case "SinWaveParticleFunction" => SinWaveParticleFunction.playFormat.reads(json)
        }

    override def writes(func: ParticleFunction1): JsValue = {
      func match {
        case func: ConstantParticleFunction => ConstantParticleFunction.playFormat.writes(func)
        case func: ParticleTimeFunction => ParticleTimeFunction.playFormat.writes(func)
        case func: ParticlePositionFunction => ParticlePositionFunction.playFormat.writes(func)
        case ParticleVelocityFunction => JsString("ParticleVelocityFunction")
        case ParticleAccelerationFunction => JsString("ParticleAccelerationFunction")
        case func: CompositeParticleFunction => CompositeParticleFunction.playFormat.writes(func)
        case func: SinWaveParticleFunction => SinWaveParticleFunction.playFormat.writes(func)
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

object SinWaveParticleFunction {
  implicit val playFormat: Format[SinWaveParticleFunction] = new Format[SinWaveParticleFunction] {
    override def reads(json: JsValue): JsResult[SinWaveParticleFunction] =
      Json.reads[SinWaveParticleFunction].reads(json)

    override def writes(func: SinWaveParticleFunction): JsValue =
      Json.writes[SinWaveParticleFunction].writes(func).deepMerge(
        Json.obj("type" -> JsString(func.getClass.getName))
      )
  }
}

case class SinWaveParticleFunction(
  offset:    Vector3D = Vector3D.ZeroValue, // m/s
  amplitude: Vector3D = Vector3D(1.0, 1.0, 1.0), // m/s
  frequency: Vector3D = Vector3D(0.01, 0.01, 0.01), // cycles/sec
  phase:     ParticleFunction1 = ParticleTimeFunction(), // radians
  theta:     ParticleFunction1 = ParticlePositionFunction()
) extends ParticleFunction1 {
  def apply(p: Particle) = offset + (amplitude * (theta(p) * frequency * 2 * Math.PI + phase(p)).sin)
}

object ConstantParticleFunction {
  implicit val playFormat: Format[ConstantParticleFunction] = new Format[ConstantParticleFunction] {
    override def reads(json: JsValue): JsResult[ConstantParticleFunction] =
      Json.reads[ConstantParticleFunction].reads(json)

    override def writes(func: ConstantParticleFunction): JsValue =
      Json.writes[ConstantParticleFunction].writes(func).deepMerge(
        Json.obj("type" -> JsString(func.getClass.getName))
      )
  }
}

/**
  * A ParticleFunction that always returns the specified Vector3D.
  */
case class ConstantParticleFunction(vector: Vector3D) extends ParticleFunction1 {
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
  * A ParticleFunction that returns a particle's time as a 3D vector.
  */
case class ParticleTimeFunction(f: Option[Vector3DFunction] = None) extends ParticleFunction1 {
  def apply(p: Particle) = f.map { func =>
    func(Vector3D(p.t, p.t, p.t))
  }.getOrElse(
    Vector3D(p.t, p.t, p.t)
  )
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
  * A function that returns a Particle's position.
  */
case class ParticlePositionFunction(f: Option[Vector3DFunction] = None) extends ParticleFunction1 {
  def apply(p: Particle) = f.map { func =>
    func(p.p)
  }.getOrElse {
    p.p
  }
}

//object ParticleVelocityFunction {
//  implicit val playFormat: Format[ParticleVelocityFunction] = new Format[ParticleVelocityFunction] {
//    override def reads(json: JsValue) = Json.reads[ParticleVelocityFunction].reads(json)
//
//    override def writes(func: ParticleVelocityFunction): JsValue = Json.writes[ParticleVelocityFunction].writes(func).deepMerge(
//      Json.obj("type" -> JsString(func.getClass.getName))
//    )
//  }
//}

/**
  * A function that returns a Particle's velocity.
  */
case object ParticleVelocityFunction extends ParticleFunction1 {
  def apply(p: Particle) = p.v
}

//object ParticleAccelerationFunction {
//  implicit val playFormat: Format[ParticleAccelerationFunction] = new Format[ParticleAccelerationFunction] {
//    override def reads(json: JsValue) = Json.reads[ParticleAccelerationFunction].reads(json)
//
//    override def writes(func: ParticleAccelerationFunction): JsValue = Json.writes[ParticleAccelerationFunction].writes(func).deepMerge(
//      Json.obj("type" -> JsString(func.getClass.getName))
//    )
//  }
//}

/**
  * A function that returns a Particle's acceleration.
  */
case object ParticleAccelerationFunction extends ParticleFunction1 {
  def apply(p: Particle) = p.a
}

object CompositeParticleFunction {
  implicit val playFormat: Format[CompositeParticleFunction] = new Format[CompositeParticleFunction] {
    override def reads(json: JsValue) = Json.reads[CompositeParticleFunction].reads(json)

    override def writes(func: CompositeParticleFunction): JsValue = Json.writes[CompositeParticleFunction].writes(func).deepMerge(
      Json.obj("type" -> JsString(func.getClass.getName))
    )
  }
}

case class CompositeParticleFunction(funcs: Seq[ParticleFunction1]) extends ParticleFunction1 {
  def apply(p: Particle) = funcs.foldLeft(Vector3D())((z, func) => z + func(p))
}
