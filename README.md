Particles
=========

A very simple particle-system simulator written in Scala.

To Do:
------

* Better/more-interactive 3D viewer.
* FunctionForce
 * Takes a Vector3DFunction and treats it as a force.
* RotatingVector3DFunction(m: Double = 1.0, b: Double = 0.0)
 * ??? apply(x: Double) = { y = m * x + b; Vector3D(Math.cos(y), Math.sin(y), 0.0) }
* Collisions
* Better code organization
* More tests
* Gradle Build
* Clean up ParticleSystemFactories

