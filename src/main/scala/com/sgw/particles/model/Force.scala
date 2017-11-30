package com.sgw.particles.model

import com.sgw.particles.model.Particle.ID
import play.api.libs.json._

/*
  TODO:

  - FunctionForce
    - Takes a function for both the magnitude and direction of the force
  - RotatingVectorFunction(m: Double = 1.0, b: Double = 0.0) - apply(x: Double) = { y = m * x + b; Vector3D(Math.cos(y), Math.sin(y), 0.0) }
  - Collision
 */

object ForceFactory {
  implicit def playFormat: Format[ForceFactory] = new Format[ForceFactory] {

    override def reads(json: JsValue): JsResult[ForceFactory] =
      (json \ "type")
        .validate[String]
        .flatMap {
          case "Gravity"            => GravityFactory.playFormat.reads(json)
          case "GravitationalForce" => GravitationalForceFactory.playFormat.reads(json)
          case "Spring"             => SpringFactory.playFormat.reads(json)
          case "ConstantForce"      => ConstantForceFactory.playFormat.reads(json)
          case "Rocket"             => RocketFactory.playFormat.reads(json)
          case "Drag"               => DragFactory.playFormat.reads(json)
          case "Damper"             => DamperFactory.playFormat.reads(json)
          case "SpringDamper"       => SpringDamperFactory.playFormat.reads(json)
        }

    override def writes(func: ForceFactory): JsValue = {
      func match {
        case factory: GravityFactory            => GravityFactory.playFormat.writes(factory)
        case factory: GravitationalForceFactory => GravitationalForceFactory.playFormat.writes(factory)
        case factory: SpringFactory             => SpringFactory.playFormat.writes(factory)
        case factory: ConstantForceFactory      => ConstantForceFactory.playFormat.writes(factory)
        case factory: RocketFactory             => RocketFactory.playFormat.writes(factory)
        case factory: DragFactory               => DragFactory.playFormat.writes(factory)
        case factory: DamperFactory             => DamperFactory.playFormat.writes(factory)
        case factory: SpringDamperFactory       => SpringDamperFactory.playFormat.writes(factory)
      }
    }
  }
}

sealed trait ForceFactory {
  def createForces(particleMap: Map[Particle.ID, Particle]): List[Force]

  protected def unknownParticleId(pId: Particle.ID): Particle = {
    throw new RuntimeException(s"Unknown particle id $pId.")
  }

  protected def getParticle(
    pId: Particle.ID,
    particleMap: Map[Particle.ID, Particle]
  ): Particle = particleMap.getOrElse(pId, unknownParticleId(pId))
}

sealed abstract class Force1Factory(pIds: List[Particle.ID]) extends ForceFactory {
  override def createForces(
    particleMap: Map[ID, Particle]
  ): List[Force] = pIds.map { pId =>
    createForce(getParticle(pId, particleMap))
  }

  protected def createForce(p1: Particle): Force
}

sealed abstract class Force2CombinationFactory(listOfListOfParticleIds: List[List[Particle.ID]]) extends ForceFactory {
  override def createForces(
    particleMap: Map[ID, Particle]
  ): List[Force] = listOfListOfParticleIds.flatMap { listOfParticleIds =>
    listOfParticleIds.map { particleId =>
      getParticle(particleId, particleMap)
    }.combinations(2).map { case List(particle1, particle2) =>
      createForce(particle1, particle2)
    }
  }

  protected def createForce(p1: Particle, p2: Particle): Force
}

/**
 * A function that applies a force to one or more particles over the specified delta time.
 */
sealed trait Force {
  var broken = false

  def force: Vector3D
  def maxForce = Double.MaxValue

  def apply: Unit
}

sealed trait Force1 extends Force {
  val p: Particle
  def apply = {
    val f = force
    broken = broken || f.len > maxForce
    if (!broken) p.a = p.a + f / p.m
  }
}

sealed trait Force2 extends Force {
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

object GravityFactory {
  implicit val playFormat: Format[GravityFactory] = new Format[GravityFactory] {
    override def reads(json: JsValue) = Json.reads[GravityFactory].reads(json)

    override def writes(f: GravityFactory): JsValue = Json.writes[GravityFactory].writes(f).deepMerge(
      Json.obj("type" -> JsString("Gravity"))
    )
  }
}

case class GravityFactory(
  pIds: List[Particle.ID],
  gv: Option[Vector3D],
) extends Force1Factory(pIds) {
  def createForce(p1: Particle): Force =
    Gravity(
      p = p1,
      gv = gv.getOrElse(Gravity.gv)
    )
}

object Gravity {
  val g = -9.81
  val gv = Vector3D(0.0, g, 0.0)
}

case class Gravity(
  p: Particle,
  gv: Vector3D = Gravity.gv
) extends Force {
  def force = gv * p.m
  def apply = p.a = p.a + gv
}

object GravitationalForceFactory {
  implicit val playFormat: Format[GravitationalForceFactory] = new Format[GravitationalForceFactory] {
    override def reads(json: JsValue) = Json.reads[GravitationalForceFactory].reads(json)

    override def writes(f: GravitationalForceFactory): JsValue = Json.writes[GravitationalForceFactory].writes(f).deepMerge(
      Json.obj("type" -> JsString("GravitationalForce"))
    )
  }
}

case class GravitationalForceFactory(
  pIds: List[List[Particle.ID]],
  bigG: Option[Double]
) extends Force2CombinationFactory(pIds) {
  override def createForce(
    p1: Particle,
    p2: Particle
  ): Force = GravitationalForce(
    p1 = p1,
    p2 = p2,
    bigG = bigG.getOrElse(GravitationalForce.bigG)
  )
}

object GravitationalForce {
  val bigG = 6.674 * Math.pow(10.0, -11)
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
  bigG: Double = GravitationalForce.bigG
) extends Force2 {
  def force = (p2.p - p1.p).normalize * bigG * p1.m * p2.m / Math.pow((p1.p - p2.p).len, 2)
}

trait SpringTrait extends Force2 {
  val springConstant: Double
  val restLength: Double
  def springForce = (p1.p - p2.p).normalize * springConstant * (restLength - (p1.p - p2.p).len)
  def force = springForce
}

object SpringFactory {
  implicit val playFormat: Format[SpringFactory] = new Format[SpringFactory] {
    override def reads(json: JsValue) = Json.reads[SpringFactory].reads(json)

    override def writes(f: SpringFactory): JsValue = Json.writes[SpringFactory].writes(f).deepMerge(
      Json.obj("type" -> JsString("Spring"))
    )
  }
}

case class SpringFactory(
  pIds: List[List[Particle.ID]],
  springConstant: Option[Double],
  restLength: Option[Double],
  maxForce: Option[Double]
) extends Force2CombinationFactory(pIds) {
  override def createForce(
    p1: Particle,
    p2: Particle
  ): Force = Spring(
    p1 = p1,
    p2 = p2,
    springConstant = springConstant.getOrElse(1.0),
    restLength = restLength.getOrElse(p1.distance(p2)),
    maxForce = maxForce.getOrElse(Double.MaxValue)
  )
}

case class Spring(
  p1: Particle,
  p2: Particle,
  springConstant: Double,
  restLength: Double,
  override val maxForce: Double = Double.MaxValue
) extends SpringTrait

object ConstantForceFactory {
  implicit val playFormat: Format[ConstantForceFactory] = new Format[ConstantForceFactory] {
    override def reads(json: JsValue) = Json.reads[ConstantForceFactory].reads(json)

    override def writes(f: ConstantForceFactory): JsValue = Json.writes[ConstantForceFactory].writes(f).deepMerge(
      Json.obj("type" -> JsString("ConstantForce"))
    )
  }
}

case class ConstantForceFactory(
  pIds: List[Particle.ID],
  forceVector: Option[Vector3D]
) extends Force1Factory(pIds) {
  override protected def createForce(
    p1: Particle
  ): Force = ConstantForce(
    p1,
    forceVector.getOrElse(Gravity.gv)
  )
}

case class ConstantForce(
  p: Particle,
  forceVector: Vector3D
) extends Force1 {
  def force = forceVector
}

object RocketFactory {
  implicit val playFormat: Format[RocketFactory] = new Format[RocketFactory] {
    override def reads(json: JsValue) = Json.reads[RocketFactory].reads(json)

    override def writes(f: RocketFactory): JsValue = Json.writes[RocketFactory].writes(f).deepMerge(
      Json.obj("type" -> JsString("Rocket"))
    )
  }
}

case class RocketFactory(
  pIds: List[Particle.ID],
  forceFunc: Option[ParticleFunction]
) extends Force1Factory(pIds) {
  override protected def createForce(
    p1: Particle
  ): Force = Rocket(
    p1,
    forceFunc.getOrElse(ConstantVector3DParticleFunction(Vector3D(y = 1.0)))
  )
}

case class Rocket(
  override val p: Particle,
  forceFunc: ParticleFunction
) extends Force1 {
  def force = forceFunc(p)
}

object DragFactory {
  implicit val playFormat: Format[DragFactory] = new Format[DragFactory] {
    override def reads(json: JsValue) = Json.reads[DragFactory].reads(json)

    override def writes(f: DragFactory): JsValue = Json.writes[DragFactory].writes(f).deepMerge(
      Json.obj("type" -> JsString("Drag"))
    )
  }
}

case class DragFactory(
  pIds: List[Particle.ID],
  fluidDensity: Option[Double],
  dragCoeff: Option[Double],
  flowFunc: Option[ParticleFunction]
) extends Force1Factory(pIds) {
  override protected def createForce(
    p1: Particle
  ): Force = Drag(
    p1,
    fluidDensity.getOrElse(0.5),
    dragCoeff.getOrElse(0.47),
    flowFunc.getOrElse(
      ConstantVector3DParticleFunction(
        vector = Vector3D.ZeroValue
      )
    )
  )
}

case class Drag(
  override val p: Particle,
  fluidDensity: Double = 0.5,
  dragCoeff: Double = 0.47,
  flowFunc: ParticleFunction = ConstantVector3DParticleFunction(
    vector = Vector3D.ZeroValue
  )
) extends Force1 {
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

object DamperFactory {
  implicit val playFormat: Format[DamperFactory] = new Format[DamperFactory] {
    override def reads(json: JsValue) = Json.reads[DamperFactory].reads(json)

    override def writes(f: DamperFactory): JsValue = Json.writes[DamperFactory].writes(f).deepMerge(
      Json.obj("type" -> JsString("Damper"))
    )
  }
}

case class DamperFactory(
  pIds: List[List[Particle.ID]],
  viscousDampingCoeff: Option[Double],
  maxForce: Option[Double]
) extends Force2CombinationFactory(pIds) {
  override protected def createForce(
    p1: Particle,
    p2: Particle
  ): Force = Damper(
    p1,
    p2,
    viscousDampingCoeff.getOrElse(0.5),
    maxForce.getOrElse(Double.MaxValue)
  )
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
  viscousDampingCoeff: Double,
  override val maxForce: Double
) extends DamperTrait

object SpringDamperFactory {
  implicit val playFormat: Format[SpringDamperFactory] = new Format[SpringDamperFactory] {
    override def reads(json: JsValue) = Json.reads[SpringDamperFactory].reads(json)

    override def writes(f: SpringDamperFactory): JsValue = Json.writes[SpringDamperFactory].writes(f).deepMerge(
      Json.obj("type" -> JsString("SpringDamper"))
    )
  }
}


case class SpringDamperFactory(
  pIds: List[List[Particle.ID]],
  springConstant: Option[Double],
  restLength: Option[Double],
  viscousDampingCoeff: Option[Double],
  maxForce: Option[Double]
) extends Force2CombinationFactory(pIds) {
  override protected def createForce(
    p1: Particle,
    p2: Particle
  ): Force = SpringDamper(
    p1,
    p2,
    springConstant.getOrElse(1.0),
    restLength.getOrElse(p1.distance(p2)),
    viscousDampingCoeff.getOrElse(0.5),
    maxForce.getOrElse(Double.MaxValue)
  )
}

case class SpringDamper(
  p1: Particle,
  p2: Particle,
  springConstant: Double,
  restLength: Double,
  viscousDampingCoeff: Double,
  override val maxForce: Double
) extends SpringTrait with DamperTrait {
  override def force = super.springForce + super.damperForce
}

