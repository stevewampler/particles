package com.sgw.particles.factories

import com.sgw.particles._
import com.sgw.particles.ParticleTimeFunction
import com.sgw.particles.Drag
import com.sgw.particles.Particle
import com.sgw.particles.SpringDamper
import com.sgw.particles.Gravity
import com.sgw.particles.ParticleSystem

/**
 * author: steve
 */
object StringParticleSystemFactory {
  def defaultPosFunction(i: Integer, spread: Double) = Vector3D(
    -spread / 2.0 + Math.random() * spread,
    spread / 2.0 + Math.random() * spread
  )
  def defaultMassFunction(i: Integer) = 1
  def defaultRadiusFunction(i: Integer) = 1
  def defaultAreaFunction(i: Integer) = 1

  def createParticleSystem(
      numParticles: Int = 6,
      spread: Int = 200,
      massFunc: (Integer) => Double = defaultMassFunction,
      radiusFunc: (Integer) => Double = defaultRadiusFunction,
      areaFunc: (Integer) => Double = defaultAreaFunction,
      force1Factories: Seq[Force1Factory],
      force2Factories: Seq[Force2Factory]) = {
    val particles = List(Particle(Double.MaxValue, Vector3D.ZeroValue)) ++
      (1 to numParticles).map(
        i => {
          Particle(
            m = massFunc(i),
            p = defaultPosFunction(i, spread),
            radius = radiusFunc(i),
            area = areaFunc(i)
          )
        }
      )

    val forces =
      particles.flatMap(particle => force1Factories.map(_.apply(particle))) :::
      particles.init.zip(particles.tail).flatMap(particlePair => force2Factories.map(force2Factory => force2Factory(particlePair._1, particlePair._2)))

    val bounds = ParticleSystemUtils.bounds(particles) * 35

    ParticleSystem(
      particles = particles,
      forces = forces,
      bounds = bounds
    )
  }
}
