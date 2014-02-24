package com.sgw.particles.factories

import com.sgw.particles.{ParticleSystemUtils, ParticleSystem, Vector3D, Particle}
import com.sgw.particles.GravitationalForce

/**
 * author: steve
 */
// Mass of Moon: 7.34767309E22 kilograms
// Distance of Earth to Moon: 384,400 km
object PlanetaryParticleSystemFactory {
  val sun = Particle()
  def createParticleSystem() = {
    val sun     = Particle(m = 1.9890E30, radius=6.955E8)
    val mercury = Particle(m = 0.3301E24, p = Vector3D(0.460E11), v = Vector3D(0.0, -5.898E4), radius = 2.440E6)
    val venus   = Particle(m = 4.8676E24, p = Vector3D(1.075E11), v = Vector3D(0.0, -3.502E4), radius = 3.396E6)
    val earth   = Particle(m = 5.9726E24, p = Vector3D(1.496E11), v = Vector3D(0.0, -2.978E4), radius = 6.371E6)
    val mars    = Particle(m = 0.6417E24, p = Vector3D(2.279E11), v = Vector3D(0.0, -2.413E4), radius = 3.396E6)
    val jupiter = Particle(m = 1898.3E24, p = Vector3D(7.405E11), v = Vector3D(0.0, -1.372E4), radius = 7.149E7)
    val saturn  = Particle(m = 568.36E24, p = Vector3D(1.353E12), v = Vector3D(0.0, -1.018E4), radius = 6.027E7)
    val uranus  = Particle(m = 86.816E24, p = Vector3D(2.741E12), v = Vector3D(0.0, -7.110E3), radius = 2.556E7)
    val neptune = Particle(m = 102.42E24, p = Vector3D(4.445E12), v = Vector3D(0.0, -5.500E3), radius = 2.476E7)
    // val pluto   = Particle(m = 0.6417E24, p = Vector3D(2.279E11), v = Vector3D(0.0, -2.413E4), radius = 3.396E6)

    val particles = sun :: mercury :: venus :: earth :: mars :: jupiter :: saturn :: uranus :: neptune :: Nil
    val forces    = particles.tail.map(planet => GravitationalForce(sun, planet))
    val bounds    = ParticleSystemUtils.bounds(particles) * 2.1

    ParticleSystem(
      particles = particles,
      forces = forces,
      bounds = bounds
    )
  }
}
