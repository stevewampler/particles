package com.sgw.particles.model

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

