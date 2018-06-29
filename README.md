Particles
=========

A simple particle-system simulator written in Scala for the fun of it.

To Run
------

Run:

com.sgw.particles.swing.ParticleSystemSimulator --file /\<model\>.json

where \<model\>.json is the name of a model file in the src/main/resoures directory.

To Do
-----

* The ParticleSystemFactory should be able to create other particles and forces on the fly.
* Bubble Forces (http://woodrowshew.com/bubjfm.pdf)
 * A BubbleParticleSystemFactory that can create bubbles of random size on the fly and destroy them when they reach the surface
* Better/more-interactive 3D viewer.
 * Select particles.
 * Drag particles.
 * Delete particles and forces.
 * Display selected object's current state.
* Collisions
* More tests
* Documentation
* Create specialized ParticleRenderers that:
 * Change the particle's color based on the particle's mass, age, velocity, acceleration, ...
 * Render a particle's history.
* Particles and Forces should be able to (optionally?) keep track of their past state as a time-series
* Make the force calculations multi-threaded
* Make the particle updates multi-threaded
* Use flink or spark to perform large particle system calcs
* FunctionForce: Takes a function for both the magnitude and direction of the force
* Make all of a force's config parameters be functions.
* RotatingVectorFunction(m: Double = 1.0, b: Double = 0.0) - apply(x: Double) = { y = m * x + b; Vector3D(Math.cos(y), Math.sin(y), 0.0) }
* Make particles deletable
 * Maybe by setting the particle's mass to zero
* Make all of a particle's config parameters be functions.
* Make forces deletable (this already works by marking a force as broken)
* Add the ability for the user to select particles.
* Add the ability for the user to drag particles.
* Add the ability for the user to delete particles and forces.
* Display a selected object's state.
* Create specialized ParticleRenderers that:
 * Change the particle's rendered size based on the mass, age, position, velocity, acceleration of the particle
 * Render a particle's destruction/creation.
 *  Render a particle's history.
* Keep the past versions of a ParticleSystem and enable them to be rendered.