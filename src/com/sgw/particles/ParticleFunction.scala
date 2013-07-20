package com.sgw.particles

/**
 * A function that, when applied to a particle, returns a Vector3D.
 */
trait ParticleFunction {
  def apply(p: Particle): Vector3D
}

/**
 * A ParticleFunction that always returns a zero length Vector3D.
 */
object ZeroVector3DParticleFunction extends ParticleFunction {
  def apply(p: Particle) = Vector3D.ZeroValue
}

/**
 * A ParticleFunction that always returns the specified Vector3D.
 */
case class ConstantVector3DParticleFunction(vector: Vector3D) extends ParticleFunction {
  def apply(p: Particle) = vector
}

/**
 * A ParticleFunction that applies the specified function to a particle's time.
 * @param func The function to be applied to a Particle's time.
 */
case class ParticleTimeFunction(func: Vector3DFunction) extends ParticleFunction {
  def apply(p: Particle) = func(Vector3D(p.t, p.t, p.t))
}

/**
 * A ParticleFunction that applies a specified Vector3DFunction to a Particle's position.
 * @param func The function to be applied to a Particle's position.
 */
case class ParticlePositionFunction(func: Vector3DFunction) extends ParticleFunction {
  def apply(p: Particle) = func(p.p)
}

/**
 * A ParticleFunction that applies a specified Vector3DFunction to a Particle's velocity.
 * @param func The function to be applied to a Particle's velocity.
 */
case class ParticleVelocityFunction(func: Vector3DFunction) extends ParticleFunction {
  def apply(p: Particle) = func(p.v)
}

/**
 * A ParticleFunction that applies a specified Vector3DFunction to a Particle's acceleration.
 * @param func The function to be applied to a Particle's acceleration.
 */
case class ParticleAccelerationFunction(func: Vector3DFunction) extends ParticleFunction {
  def apply(p: Particle) = func(p.a)
}

case class CompositeParticleFunction(funcs: Seq[ParticleFunction]) {
  def apply(p: Particle) = funcs.foldLeft(Vector3D())((z, func) => z + func(p))
}
