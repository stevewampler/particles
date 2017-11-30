package com.sgw.particles.model

import play.api.libs.json._

//-------------------------
// Vector3D Functions

object Vector3DFunction {
  implicit def playFormat: Format[Vector3DFunction] = new Format[Vector3DFunction] {

    override def reads(json: JsValue): JsResult[Vector3DFunction] =
      (json \ "type")
        .validate[String]
        .flatMap {
          case "ConstantVector3DFunction"    => ConstantVector3DFunction.playFormat.reads(json)
          case "RandomVector3DFunction"      => RandomVector3DFunction.playFormat.reads(json)
          case "SinWaveVector3DFunction"     => SinWaveVector3DFunction.playFormat.reads(json)
          case "NormalVector3DFunction"      => NormalVector3DFunction.playFormat.reads(json)
          case "NormalizeVector3DFunction"   => NormalizeVector3DFunction.playFormat.reads(json)
          case "TranslateVector3D"           => TranslateVector3D.playFormat.reads(json)
          case "ScaleVector3DFunction"       => ScaleVector3DFunction.playFormat.reads(json)
          case "LinearScaleVector3DFunction" => LinearScaleVector3DFunction.playFormat.reads(json)
          case "CompositeVector3DFunction"   => CompositeVector3DFunction.playFormat.reads(json)
        }

    override def writes(func: Vector3DFunction): JsValue = {
      func match {
        case func: ConstantVector3DFunction    => ConstantVector3DFunction.playFormat.writes(func)
        case func: RandomVector3DFunction      => RandomVector3DFunction.playFormat.writes(func)
        case func: SinWaveVector3DFunction     => SinWaveVector3DFunction.playFormat.writes(func)
        case func: NormalVector3DFunction      => NormalVector3DFunction.playFormat.writes(func)
        case func: NormalizeVector3DFunction   => NormalizeVector3DFunction.playFormat.writes(func)
        case func: TranslateVector3D           => TranslateVector3D.playFormat.writes(func)
        case func: ScaleVector3DFunction       => ScaleVector3DFunction.playFormat.writes(func)
        case func: LinearScaleVector3DFunction => LinearScaleVector3DFunction.playFormat.writes(func)
        case func: CompositeVector3DFunction   => CompositeVector3DFunction.playFormat.writes(func)
      }
    }
  }
}

trait Vector3DFunction {
  def apply(v: Vector3D): Vector3D
}

object ConstantVector3DFunction {
  implicit val playFormat: Format[ConstantVector3DFunction] = new Format[ConstantVector3DFunction] {
    override def reads(json: JsValue) = Json.reads[ConstantVector3DFunction].reads(json)

    override def writes(func: ConstantVector3DFunction): JsValue = Json.writes[ConstantVector3DFunction].writes(func).deepMerge(
      Json.obj("type" -> JsString(func.getClass.getName))
    )
  }
}

case class ConstantVector3DFunction(const: Vector3D) extends Vector3DFunction {
  def apply(v: Vector3D): Vector3D = const
}

object RandomVector3DFunction {
  implicit val playFormat: Format[RandomVector3DFunction] = new Format[RandomVector3DFunction] {
    override def reads(json: JsValue) = Json.reads[RandomVector3DFunction].reads(json)

    override def writes(func: RandomVector3DFunction): JsValue = Json.writes[RandomVector3DFunction].writes(func).deepMerge(
      Json.obj("type" -> JsString(func.getClass.getName))
    )
  }
}

case class RandomVector3DFunction(minFunc: Vector3DFunction, maxFunc: Vector3DFunction) extends Vector3DFunction {
  def apply(v: Vector3D): Vector3D = {
    val max = maxFunc(v)
    val min = minFunc(v)
    val rand = Math.random()
    Vector3D(
      rand * (max.x - min.x) + min.x,
      rand * (max.y - min.y) + min.y,
      rand * (max.z - min.z) + min.z)
  }
}

object SinWaveVector3DFunction {
  implicit val playFormat: Format[SinWaveVector3DFunction] = new Format[SinWaveVector3DFunction] {
    override def reads(json: JsValue) = Json.reads[SinWaveVector3DFunction].reads(json)

    override def writes(func: SinWaveVector3DFunction): JsValue = Json.writes[SinWaveVector3DFunction].writes(func).deepMerge(
      Json.obj("type" -> JsString(func.getClass.getName))
    )
  }
}

case class SinWaveVector3DFunction(
  offset: Vector3D = Vector3D(10.0), // m/s
  amplitude: Vector3D = Vector3D(50.0), // m/s
  frequency: Vector3D = Vector3D(0.25, 1.0, 1.0), // cycles/sec
  phase: Vector3D = Vector3D() // radians
) extends Vector3DFunction {
  def apply(theta: Vector3D) = offset + (amplitude * (theta * frequency * 2 * Math.PI + phase).sin)
}

object NormalVector3DFunction {
  implicit def playFormat[C]: Format[NormalVector3DFunction] = new Format[NormalVector3DFunction] {
    override def reads(json: JsValue) = JsSuccess(NormalVector3DFunction())

    override def writes(func: NormalVector3DFunction): JsValue = Json.obj("type" -> JsString(func.getClass.getName))
  }
}

case class NormalVector3DFunction() extends Vector3DFunction {
  def apply(v: Vector3D) = Vector3D(-v.y, v.x)
}

object NormalizeVector3DFunction {
  implicit def playFormat[C]: Format[NormalizeVector3DFunction] = new Format[NormalizeVector3DFunction] {
    override def reads(json: JsValue) = JsSuccess(NormalizeVector3DFunction())

    override def writes(func: NormalizeVector3DFunction): JsValue = Json.obj("type" -> JsString(func.getClass.getName))
  }
}

case class NormalizeVector3DFunction() extends Vector3DFunction {
  def apply(v: Vector3D) = v.normalize
}

object TranslateVector3D {
  implicit val playFormat: Format[TranslateVector3D] = new Format[TranslateVector3D] {
    override def reads(json: JsValue) = Json.reads[TranslateVector3D].reads(json)

    override def writes(func: TranslateVector3D): JsValue = Json.writes[TranslateVector3D].writes(func).deepMerge(
      Json.obj("type" -> JsString(func.getClass.getName))
    )
  }
}

case class TranslateVector3D(to: Vector3D) extends Vector3DFunction {
  def apply(v: Vector3D) = v + to
}

object ScaleVector3DFunction {
  implicit val playFormat: Format[ScaleVector3DFunction] = new Format[ScaleVector3DFunction] {
    override def reads(json: JsValue) = Json.reads[ScaleVector3DFunction].reads(json)

    override def writes(func: ScaleVector3DFunction): JsValue = Json.writes[ScaleVector3DFunction].writes(func).deepMerge(
      Json.obj("type" -> JsString(func.getClass.getName))
    )
  }
}

case class ScaleVector3DFunction(s: Double) extends Vector3DFunction {
  def apply(v: Vector3D) = v * s
}

object LinearScaleVector3DFunction {
  implicit val playFormat: Format[LinearScaleVector3DFunction] = new Format[LinearScaleVector3DFunction] {
    override def reads(json: JsValue) = Json.reads[LinearScaleVector3DFunction].reads(json)

    override def writes(func: LinearScaleVector3DFunction): JsValue = Json.writes[LinearScaleVector3DFunction].writes(func).deepMerge(
      Json.obj("type" -> JsString(func.getClass.getName))
    )
  }
}

case class LinearScaleVector3DFunction(m: Double, b: Double) extends Vector3DFunction {
  def apply(v: Vector3D) = v * m + v.normalize * b
}

object CompositeVector3DFunction {
  implicit val playFormat: Format[CompositeVector3DFunction] = new Format[CompositeVector3DFunction] {
    override def reads(json: JsValue) = Json.reads[CompositeVector3DFunction].reads(json)

    override def writes(func: CompositeVector3DFunction): JsValue = Json.writes[CompositeVector3DFunction].writes(func).deepMerge(
      Json.obj("type" -> JsString(func.getClass.getName))
    )
  }
}

case class CompositeVector3DFunction(funcs: Seq[Vector3DFunction]) extends Vector3DFunction {
  def apply(v: Vector3D) = funcs.foldLeft(v)((v, func) => func(v))
}
