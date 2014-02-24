Particles
=========

A very simple particle-system simulator written in Scala for the fun of it.

To Do:
------

* The ParticleSystemFactory should be able to create other particles and forces on the fly.
* Bubble Forces (http://woodrowshew.com/bubjfm.pdf)
 * A BubbleParticleSystemFactory that can create bubbles of random size on the fly and destroy them when they reach the surface
* Better/more-interactive 3D viewer.
 * Select particles.
 * Drag particles.
 * Delete particles and forces.
 * Display selected object's current state.
* Collisions
* Better code organization
* More tests
* Documentation
* Gradle Build
* Clean up ParticleSystemFactories
* Create specialized ParticleRenderers that:
 * Change the particle's color based on the particle's mass, age, velocity, acceleration, ...
 * Render a particle's history.
* Particles and Forces should be able to (optionally?) keep track of their past state as a time-series

