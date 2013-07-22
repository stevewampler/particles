package com.sgw.particles

//-------------------------
// Force Factories

trait Force1Factory {
  def apply(p: Particle): Force1
}

trait Force2Factory {
  def apply(p1: Particle, p2: Particle): Force2
}

case class GravityFactory(g: Double = -9.81) extends Force1Factory {
  def apply(p: Particle) = Gravity(p, g)
}

case class GravitationalForceFactory(bigG: Double = 6.674 * Math.pow(10.0, -11)) extends Force2Factory {
  def apply(p1: Particle, p2: Particle) = GravitationalForce(p1, p2, bigG)
}

case class SpringFactory(springConstant: Double, restLength: Double, maxForce: Double = Double.MaxValue) extends Force2Factory {
  def apply(p1: Particle, p2: Particle) = Spring(p1, p2, springConstant, restLength, maxForce)
}

case class ConstantForceFactory(forceVector: Vector3D) extends Force1Factory {
  def apply(p: Particle) = ConstantForce(p, forceVector)
}

case class RocketFactory(forceFunc: ParticleFunction) extends Force1Factory {
  def apply(p: Particle) = Rocket(p, forceFunc)
}

case class DragFactory(fluidDensity: Double = 0.5, dragCoeff: Double = 0.47, flowFunc: ParticleFunction = ZeroVector3DParticleFunction) extends Force1Factory {
  def apply(p: Particle) = Drag(p, fluidDensity, dragCoeff, flowFunc)
}

case class DamperFactory(viscousDampingCoeff: Double = 0.5, maxForce: Double = Double.MaxValue) extends Force2Factory {
  def apply(p1: Particle, p2: Particle) = Damper(p1, p2, viscousDampingCoeff, maxForce)
}

case class SpringDamperFactory(springConstant: Double, restLength: Double, viscousDampingCoeff: Double, maxForce: Double = Double.MaxValue) extends Force2Factory {
  def apply(p1: Particle, p2: Particle) = SpringDamper(p1, p2, springConstant, restLength, viscousDampingCoeff, maxForce)
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
  def apply(): Unit
}

trait Force1 extends Force {
  val p: Particle
  def apply() = {
    val f = force
    broken = broken || f.len > maxForce
    if (!broken) p.a = p.a + f / p.m
  }
}

trait Force2 extends Force {
  val p1: Particle
  val p2: Particle
  def apply() = {
    val f = force
    broken = broken || f.len > maxForce
    if (!broken) {
      p1.a = p1.a + f / p1.m
      p2.a = p2.a - f / p2.m
    }
  }
}

case class Gravity(p: Particle, g: Double = -9.81) extends Force1 {
  private val gv = Vector3D(0.0, g, 0.0)
  def force = gv * p.m
  override def apply() = p.a = p.a + gv
}

// Mass of Sun: 1.989E30 kg
// Distance of Earth to Sun: 149,600,000 km
// Mass of Mars: 639E21 kg
// Distance of Mars to Sun: 227,900,000 km
// Mass of Earth: 5.972E24 kg
// Mass of Moon: 7.34767309E22 kilograms
// Distance of Earth to Moon: 384,400 km
case class GravitationalForce(
    p1: Particle,
    p2: Particle,
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

