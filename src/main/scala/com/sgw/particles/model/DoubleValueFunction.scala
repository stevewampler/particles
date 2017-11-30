package com.sgw.particles.model

import play.api.libs.json._

object DoubleValueFunction {
  implicit def playFormat: Format[DoubleValueFunction] = new Format[DoubleValueFunction] {

    override def reads(json: JsValue): JsResult[DoubleValueFunction] =
      (json \ "type")
        .validate[String]
        .flatMap {
          case "ConstantFunc"        => ConstantFunc.playFormat.reads(json)
          case "RandomValueFunction" => RandomValueFunction.playFormat.reads(json)
        }

    override def writes(func: DoubleValueFunction): JsValue = {
      func match {
        case func: ConstantFunc        => ConstantFunc.playFormat.writes(func)
        case func: RandomValueFunction => RandomValueFunction.playFormat.writes(func)
      }
    }
  }
}

trait DoubleValueFunction {
  def apply(x: Double): Double
}

object ConstantFunc {
  implicit val playFormat: Format[ConstantFunc] = new Format[ConstantFunc] {
    override def reads(json: JsValue) = Json.reads[ConstantFunc].reads(json)

    override def writes(func: ConstantFunc): JsValue = Json.writes[ConstantFunc].writes(func).deepMerge(
      Json.obj("type" -> JsString(func.getClass.getName))
    )
  }
}

case class ConstantFunc(const: Double) extends DoubleValueFunction {
  def apply(x: Double) = const
}

object RandomValueFunction {
  implicit val playFormat: Format[RandomValueFunction] = new Format[RandomValueFunction] {
    override def reads(json: JsValue) = Json.reads[RandomValueFunction].reads(json)

    override def writes(func: RandomValueFunction): JsValue = Json.writes[RandomValueFunction].writes(func).deepMerge(
      Json.obj("type" -> JsString(func.getClass.getName))
    )
  }
}

case class RandomValueFunction(minFunc: DoubleValueFunction, maxFunc: DoubleValueFunction) extends DoubleValueFunction {
  def apply(x: Double) = Math.random() * (maxFunc(x) - minFunc(x)) + minFunc(x)
}
