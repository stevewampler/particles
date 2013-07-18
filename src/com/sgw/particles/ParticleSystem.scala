package com.sgw.particles

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

case class ParticleSystem(particles: Seq[Particle], forces: Seq[Force], bounds: Bounds3D)

