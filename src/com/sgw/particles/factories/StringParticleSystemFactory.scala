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
  def createParticleSystem(numParticles: Int = 6, spread: Int = 200) = {
    val particles = List(Particle(Double.MaxValue, Vector3D.ZeroValue)) ++
      (1 to numParticles).map(
        i => {
          Particle(
            m = i,
            p = Vector3D(
              -spread / 2.0 + Math.random() * spread,
              spread / 2.0 + Math.random() * spread
            ),
            radius = i,
            area = 1
          )
        }
      )

    val waveFunction = SinWaveVector3DFunction(
      offset     = Vector3D(0.0), // m/s
      amplitude  = Vector3D(50.0), // m/s
      frequency  = Vector3D(0.25, 1.0, 1.0), // cycles/sec
      phase      = Vector3D() // radians
    )

    val flowFunc = ParticleTimeFunction(waveFunction)

    // connect all of the particles with springs
    val forces =
      particles.tail.map(particle => Gravity(particle)) :::
      particles.init.zip(particles.tail).map(pair => SpringDamper(pair._1, pair._2, 5, 10, 1, 500)) :::
      particles.tail.map(particle => Drag(particle, flowFunc = flowFunc))
//        particles.init.zip(particles.tail).map(pair => Damper(pair._1, pair._2, 1)) :::
    /*
        particles.tail.map(
          particle => ConstantForce(particle, Vector3D(20, 20, 0))
        ) :::
        */
//        particles.tail.map(particle => Drag(particle, flowFunc = ConstantVector3DParticleFunction(Vector3D(20, 0, 0))))

    // :::
    // particles.tail.map(particle => Drag(particle, flowFunc = ParticlePositionFunction(ConstantVector3DFunction(Vector3D(50, 0, 0)))))
    /*
    particles.tail.map(
      particle => Drag(
        particle,
        flowFunc = ParticlePositionFunction(
          SinVector3DFunction(
            freqFunc = ConstantFunc(10.0),
            vectorFreqFunc = ConstantVector3DFunction(
              Vector3D(Double.MaxValue, Double.MaxValue, Double.MaxValue)
            ),
            minFunc = ConstantVector3DFunction(
              Vector3D(0, 0, 0)
            ),
            maxFunc = ConstantVector3DFunction(
              Vector3D(0, 100, 0)
            )
          )
        )
      )
    )
    */
    /*
    particles.tail.map(
      particle => Drag(
        particle,
        flowFunc = ParticlePositionFunction(
          CompositeVector3DFunction(
            List(
              NormalVector3DFunction,
              NormalizeVector3DFunction,
              LinearScaleVector3DFunction(100, 0)
            )
          )
        )
      )
    )
    */

    val bounds = ParticleSystemUtils.bounds(particles) * 35

    ParticleSystem(
      particles,
      forces,
      bounds
    )
  }
}
