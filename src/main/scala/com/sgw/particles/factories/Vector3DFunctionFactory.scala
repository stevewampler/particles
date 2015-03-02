package com.sgw.particles.factories

import com.sgw.particles.{SinWaveVector3DFunction, Vector3DFunction}
import com.sgw.particles.utils.JSON

object Vector3DFunctionFactory {
  def apply(config: JSON): Option[Vector3DFunction] = {
    config.getString("type").map {
      case "SinWaveVector3DFunction" => SinWaveVector3DFunction(config)
    }
  }
}
