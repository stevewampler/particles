package com.sgw.particles.factories

import com.sgw.particles.utils.JSON
import com.sgw.particles.{ParticleSystemUtils, ParticleSystem}

object SimpleParticleSystemFactory extends ParticleSystemFactory {
  val name = "Simple"

  def apply(config: JSON): ParticleSystem = {
    val particlesMap = config.getParticlesMap("particles").getOrElse(Map())
    val particles = particlesMap.values.toList
    val forces = config.getForcesList("forces", particlesMap).getOrElse(List()).flatten
    val bounds = config.getBounds3D("bounds").getOrElse(ParticleSystemUtils.bounds(particles) * 10)

    ParticleSystem(
      particles,
      forces,
      bounds
    )
  }
}