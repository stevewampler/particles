package com.sgw.particles.factories

import com.sgw.particles.utils.{Loggable, JSON}
import com.sgw.particles.{SpringDamper, Gravity, Force, Particle}

object ForceFactory extends Loggable {
  def apply(config: JSON, particlesMap: Map[String, Particle]): Option[List[Force]] = {
    config.getString("type").flatMap {
      case "Gravity" => Gravity(config, particlesMap)
      case "SpringDamper" => SpringDamper(config, particlesMap)
      case "Drag" => None
      case _ => {
        error("Unknown force type '" + config.getString("type") + "'. Skipping!")
        None
      }
    }
  }
}
