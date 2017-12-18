package com.sgw.particles.model

/*
  TODO:

  - Make particles deletable
    particle.delete()
      fires an onDeleted event to the ParticleSystem, Forces, and Renderer
  - Make forces deletable
    force.delete()
      fires an onDeleted event to the ParticleSystem and to its Renderer
  - Particles and Forces should also be able to create other particles and forces.
    - They should fire an event to the ParticleSystem.
  - The ParticleSystemFactor might also want to create other particles and forces on the fly.
  - Add the ability for the user to select particles.
  - Add the ability for the user to drag particles.
  - Add the ability for the user to delete particles and forces.
  - Display selected object's info somewhere.
  - Change the simulation loop:
    - Simulation time should be able to be tied to real time.
  - Create specialized ParticleRenderers that:
    - Change the particle's rendered size based on the mass, age, position, velocity, acceleration of the particle
    - Render a particle's destruction/creation.
    - Render a particle's history.
  - Change the Particles and Forces to keep track of it's past state (i.e. as a time-series)
  - Create a NullRenderer that doesn't do anything and can be filtered out of the renderers list and used to NOT render
    a particle or force.
 */

object ParticleSystem {

  def createFromFactories(
    name: String,
    particleFactories: List[ParticleFactory],
    forceFactories: List[ForceFactory],
    maybeBounds: Option[Bounds3D]
  ): ParticleSystem = {
    val particles = particleFactories.foldLeft(List[Particle]()) { case (acc, particleFactory) =>
      acc ++ particleFactory.createParticles
    }

    val particleMap = particles.map { particle =>
      particle.id -> particle
    }.toMap

    val forces = forceFactories.foldLeft(List[Force]()) { case (acc, forceFactory) =>
      acc ++ forceFactory.createForces(particleMap)
    }

    ParticleSystem(
      name,
      particles,
      forces,
      maybeBounds
    )
  }

  def apply(
    name: String,
    particles: List[Particle],
    forces: List[Force],
    maybeBounds: Option[Bounds3D]
  ): ParticleSystem = {
    val particleMap = particles.map { particle =>
      particle.id -> particle
    }.toMap

    val forceMap = forces.map { force =>
      force.id -> force
    }.toMap

    ParticleSystem(
      name,
      particleMap,
      forceMap,
      maybeBounds.getOrElse(
        ParticleSystemUtils.bounds(particles) * 10
      )
    )
  }
}

case class ParticleSystem(
  name: String,
  particleMap: Map[Particle.ID, Particle],
  forceMap: Map[Force.ID, Force],
  bounds: Bounds3D
) {
  def initForces: ParticleSystem =
    particleMap.values.foldLeft(this) { case (pSys, particle) =>
      particle.initForces(pSys)
    }

  def apply(dt: Double): ParticleSystem = calcForces.moveParticles(dt)

  private def calcForces: ParticleSystem =
    forceMap.values.foldLeft(this) { case (pSys, force) =>
      force(pSys)
    }

  private def moveParticles(dt: Double): ParticleSystem =
    particleMap.values.foldLeft(this) { case (pSys, particle) =>
      particle(pSys)(dt)
    }

  def maybeGetParticle(pId: Particle.ID): Option[Particle] = particleMap.get(pId)

  def getParticleOrElse[B >: Particle](pId: Particle.ID, default: => B): B = maybeGetParticle(pId).getOrElse(default)

  def getParticle(pId: Particle.ID): Particle = getParticleOrElse(
    pId,
    throw new RuntimeException(s"Unknown particle id $pId!")
  )
}

