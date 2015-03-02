package com.sgw.particles.factories

import com.sgw.particles.utils.JSON
import com.sgw.particles.ParticleSystem

trait ParticleSystemFactory {
  val name: String
  def apply(config: JSON): ParticleSystem
}
