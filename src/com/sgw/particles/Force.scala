package com.sgw.particles

/*
  TODO:

  - FunctionForce
    - Takes a function for both the magnitude and direction of the force
  - RotatingVectorFunction(m: Double = 1.0, b: Double = 0.0) - apply(x: Double) = { y = m * x + b; Vector3D(Math.cos(y), Math.sin(y), 0.0) }
  - Collision
  - Make Springs breakable.
 */

//---------------------------
// Double Functions

trait DoubleValueFunction {
  def apply(x: Double): Double
}

case class ConstantFunc(const: Double) extends DoubleValueFunction {
  def apply(x: Double) = const
}

case class RandomValueFunction(minFunc: DoubleValueFunction, maxFunc: DoubleValueFunction) extends DoubleValueFunction {
  def apply(x: Double) = Math.random() * (maxFunc(x) - minFunc(x)) + minFunc(x)
}

//-------------------------
// Vector3D Functions

trait Vector3DFunction {
  def apply(v: Vector3D): Vector3D
}

case class ConstantVector3DFunction(const: Vector3D) extends Vector3DFunction {
  def apply(v: Vector3D) = const
}

case class RandomVector3DFunction(minFunc: Vector3DFunction, maxFunc: Vector3DFunction) extends Vector3DFunction {
  def apply(v: Vector3D) = {
    val max = maxFunc(v)
    val min = minFunc(v)
    val rand = Math.random()
    Vector3D(
      rand * (max.x - min.x) + min.x,
      rand * (max.y - min.y) + min.y,
      rand * (max.z - min.z) + min.z)
  }
}

case class SinWaveVector3DFunction(
  offset: Vector3D = Vector3D(10.0), // m/s
  amplitude: Vector3D = Vector3D(50.0), // m/s
  frequency: Vector3D = Vector3D(0.25, 1.0, 1.0), // cycles/sec
  phase: Vector3D = Vector3D() // radians
) extends Vector3DFunction {
  def apply(theta: Vector3D) = offset + (amplitude * (theta * frequency * 2 * Math.PI + phase).sin)
}

object NormalVector3DFunction extends Vector3DFunction {
  def apply(v: Vector3D) = Vector3D(-v.y, v.x)
}

object NormalizeVector3DFunction extends Vector3DFunction {
  def apply(v: Vector3D) = v.normalize
}

case class TranslateVector3D(to: Vector3D) extends Vector3DFunction {
  def apply(v: Vector3D) = v + to
}


case class ScaleVector3DFunction(s: Double) extends Vector3DFunction {
  def apply(v: Vector3D) = v * s
}

case class LinearScaleVector3DFunction(m: Double, b: Double) extends Vector3DFunction {
  def apply(v: Vector3D) = v * m + v.normalize * b
}

case class CompositeVector3DFunction(funcs: Seq[Vector3DFunction]) extends Vector3DFunction {
  def apply(v: Vector3D) = funcs.foldLeft(v)((v, func) => func(v))
}

//------------------------
// Particle Functions

/**
 * A function that, when applied to a particle, returns a Vector3D.
 */
trait ParticleFunction {
  def apply(p: Particle): Vector3D
}

/*
case class SinWaveParticleFunction(
    offset: Vector3D = Vector3D(10.0), // m/s
    amplitude: Vector3D = Vector3D(50.0), // m/s
    frequency: Vector3D = Vector3D(0.25, 1.0, 1.0), // cycles/sec
    phase: Vector3D = Vector3D()
    // posPeriod: Vector3D = Vector3D(200.0, 0.0, 0.0), // m
    // timePeriod: Double = 4.0 // s
) extends ParticleFunction {
  /*
  val posMult = Vector3D(
    if (posPeriod.x != 0.0) (2.0 * Math.PI) / posPeriod.x else 0.0,
    if (posPeriod.y != 0.0) (2.0 * Math.PI) / posPeriod.y else 0.0,
    if (posPeriod.z != 0.0) (2.0 * Math.PI) / posPeriod.z else 0.0
  )
  */
  // val timeMult = 2.0 * Math.PI / timePeriod
  def apply(p: Particle) = offset + (amplitude * (frequency * 2 * Math.PI * p.t + phase).sin)
}
*/
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

//-------------------------
// Forces

/**
 * A function that applies a force to one or more particles over the specified delta time.
 */
trait Force {
  var broken = false
  def force: Vector3D
  def maxForce = Double.MaxValue
  def apply: Unit
}

trait Force1 extends Force {
  val p: Particle
  def apply = {
    val f = force
    broken = broken || f.len > maxForce
    if (!broken) p.a = p.a + f / p.m
  }
}

trait Force2 extends Force {
  val p1: Particle
  val p2: Particle
  def apply = {
    val f = force
    broken = broken || f.len > maxForce
    if (!broken) {
      p1.a = p1.a + f / p1.m
      p2.a = p2.a - f / p2.m
    }
  }
}

case class Gravity(p: Particle, g: Double = -9.81) extends Force {
  private val gv = Vector3D(0.0, g, 0.0)
  def force = gv * p.m
  def apply = p.a = p.a + gv
}

// Mass of Sun: 1.989E30 kg
// Distance of Earth to Sun: 149,600,000 km
// Mass of Mars: 639E21 kg
// Distance of Mars to Sun: 227,900,000 km
// Mass of Earth: 5.972E24 kg
// Mass of Moon: 7.34767309E22 kilograms
// Distance of Earth to Moon: 384,400 km
case class GravitationalForce(
    val p1: Particle,
    val p2: Particle,
    bigG: Double = 6.674 * Math.pow(10.0, -11)) extends Force2 {
  def force = {
    val value = (p2.p - p1.p).normalize * bigG * p1.m * p2.m / Math.pow((p1.p - p2.p).len, 2)
    value
  }
}

trait SpringTrait extends Force2 {
  val springConstant: Double
  val restLength: Double
  def springForce = (p1.p - p2.p).normalize * springConstant * (restLength - (p1.p - p2.p).len)
  def force = springForce
}

case class Spring(
    p1: Particle,
    p2: Particle,
    springConstant: Double,
    restLength: Double,
    override val maxForce: Double = Double.MaxValue) extends SpringTrait

case class ConstantForce(
    p: Particle,
    forceVector: Vector3D) extends Force1 {
  def force = forceVector
}

case class Rocket(
    override val p: Particle,
    forceFunc: ParticleFunction) extends Force1 {
  def force = forceFunc(p)
}

case class Drag(
    override val p: Particle,
    fluidDensity: Double = 0.5,
    dragCoeff: Double = 0.47,
    flowFunc: ParticleFunction = ZeroVector3DParticleFunction) extends Force1 {
  // m^2 / s^2 * kg / m^3 * m^2 = (kg * m) / s^2
  def force = {
    val flow = flowFunc(p)
    val relativeFlow = flow - p.v
    val relativeSpeed = relativeFlow.len
    relativeFlow.normalize * (0.5 * fluidDensity * relativeSpeed * relativeSpeed * dragCoeff * p.area)
  }
}

trait DamperTrait extends Force2 {
  val viscousDampingCoeff: Double
  def damperForce = (p1.v - p2.v).projectOnTo(p1.p - p2.p) * -viscousDampingCoeff
  def force = damperForce
}

/**
 * A damping force between two particles.
 * <pre>
 * F = -c v
 * </pre>
 * where c is the viscous damping coefficient in units of newton seconds per meter (N s/m)
 * and v is the relative velocity between the two particles.
 *
 * @param p1 particle 1
 * @param p2 particle 2
 * @param viscousDampingCoeff the viscous damping coefficient
 */
case class Damper(
    override val p1: Particle,
    override val p2: Particle,
    viscousDampingCoeff: Double = 0.5,
    override val maxForce: Double = Double.MaxValue) extends DamperTrait

case class SpringDamper(
    p1: Particle,
    p2: Particle,
    springConstant: Double,
    restLength: Double,
    viscousDampingCoeff: Double,
    override val maxForce: Double = Double.MaxValue) extends SpringTrait with DamperTrait {
  override def force = super.springForce + super.damperForce
}

