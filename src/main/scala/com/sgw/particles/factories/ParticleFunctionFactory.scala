package com.sgw.particles.factories

import com.sgw.particles.{ParticleTimeFunction, ParticleFunction}
import com.sgw.particles.utils.{Loggable, JSON}

object ParticleFunctionFactory extends Loggable {
  def apply(config: JSON): Option[ParticleFunction] = {
    config.getString("type").map {
      case "ParticleTimeFunction" => ParticleTimeFunction(config)
    }
  }
}
