package com.sgw.particles.model

import java.util.concurrent.atomic.AtomicLong

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
    particleMap: Map[Particle.ID, Particle]
  )(
    pId: Particle.ID
  ): Particle = particleMap.getOrElse(pId, unknownParticleId(pId))
}

sealed abstract class Force1Factory(pIds: List[Particle.ID]) extends ForceFactory {
  override def createForces(particleMap: Map[Particle.ID, Particle]): List[Force] =
    pIds.map(getParticle(particleMap)).map(createForce)

  protected def createForce(particle: Particle): Force1
}

sealed abstract class Force2Factory(p1Id: Particle.ID, p2Id: Particle.ID) extends ForceFactory {
  override def createForces(particleMap: Map[Particle.ID, Particle]): List[Force] =
    List(
      createForce(
        getParticle(particleMap)(p1Id),
        getParticle(particleMap)(p2Id)
      )
    )

  protected def createForce(p1: Particle, p2: Particle): Force1
}

sealed abstract class Force2CombinationFactory(
  pIds: List[List[Particle.ID]]
) extends ForceFactory {
  override def createForces(particleMap: Map[Particle.ID, Particle]): List[Force2] =
    pIds.flatMap { listOfParticleIds =>
      listOfParticleIds.combinations(2).flatMap { case List(p1Id, p2Id) =>
        createForces(
          getParticle(particleMap)(p1Id),
          getParticle(particleMap)(p2Id)
        )
      }
    }

  protected def createForces(p1: Particle, p2: Particle): List[Force2]
}

object Force {
  type ID = Long

  private val _nextForceId: AtomicLong = new AtomicLong(-1)

  def nextForceId: ID = _nextForceId.incrementAndGet()
}

/**
 * A function that applies a force to a specified particle over the specified delta time.
 */
sealed trait Force {
  val id: Force.ID
  val maxForce: Double // the maximum magnitude of this force's force vector
  val value: Vector3D

  def apply(pSys: ParticleSystem): ParticleSystem
}

case class Force1(
  id: Force.ID,
  pId: Particle.ID,
  forceFunc: ParticleFunction1,
  maxForce: Double = Double.MaxValue,
  value: Vector3D = Vector3D.ZeroValue
) extends Force {

  def apply(pSys: ParticleSystem): ParticleSystem = {
    val p = pSys.getParticle(pId)

    val fVector = forceFunc(p)

    // if the force is broken ...
    if (fVector.len > maxForce) {
      // remove the force from the particle system and don't update the particle's force vector
      pSys.copy(
        forceMap = pSys.forceMap - id
      )
    } else {
      // update the particle's force vector
      pSys.copy(
        particleMap = pSys.particleMap.updated(
          p.id,
          p.copy(
            f1 = p.f1 + fVector
          )
        ),
        forceMap = pSys.forceMap.updated(
          id,
          copy(
            value = fVector
          )
        )
      )
    }
  }
}

case class Force2(
  id: Force.ID,
  p1Id: Particle.ID,
  p2Id: Particle.ID,
  forceFunc: ParticleFunction2,
  maxForce: Double = Double.MaxValue,
  value: Vector3D = Vector3D.ZeroValue
) extends Force {

  def apply(pSys: ParticleSystem): ParticleSystem = {
    val p1 = pSys.getParticle(p1Id)
    val p2 = pSys.getParticle(p2Id)

    val fVector = forceFunc(p1, p2)

    // if the force is broken ...
    if (fVector.len > maxForce) {
      // remove the force from the particle system and don't update the particle's force vector
      pSys.copy(
        forceMap = pSys.forceMap - id
      )
    } else {
      // update both particle's force vector
      pSys.copy(
        particleMap = pSys.particleMap.updated(
          p1.id,
          p1.copy(
            f1 = p1.f1 + fVector
          )
        ).updated(
          p2.id,
          p2.copy(
            f1 = p2.f1 - fVector
          )
        ),
        forceMap = pSys.forceMap.updated(
          id,
          copy(
            value = fVector
          )
        )
      )
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
  gv: Option[Vector3D] = None
) extends Force1Factory(pIds) {
  protected def createForce(particle: Particle): Force1 =
    Force1(
      id = Force.nextForceId,
      pId = particle.id,
      forceFunc = Gravity(gv.getOrElse(Gravity.gv)),
    )
}

object Gravity {
  val g: Double = -9.81
  val gv: Vector3D = Vector3D(0.0, g)
}

case class Gravity(
  gv: Vector3D = Gravity.gv
) extends ParticleFunction1 {
  def apply(p: Particle): Vector3D = gv * p.m
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
  listOfListOfParticleIds: List[List[Particle.ID]],
  bigG: Option[Double] = None
) extends Force2CombinationFactory(listOfListOfParticleIds) {
  override def createForces(
    p1: Particle,
    p2: Particle
  ): List[Force2] = List(
    Force2(
      id = Force.nextForceId,
      p1Id = p1.id,
      p2Id = p2.id,
      GravitationalForce(
        bigG = bigG.getOrElse(GravitationalForce.bigG)
      )
    )
  )
}

object GravitationalForce {
  val bigG: Double = 6.674 * Math.pow(10.0, -11)
}

// Mass of Sun: 1.989E30 kg
// Distance of Earth to Sun: 149,600,000 km
// Mass of Mars: 639E21 kg
// Distance of Mars to Sun: 227,900,000 km
// Mass of Earth: 5.972E24 kg
// Mass of Moon: 7.34767309E22 kilograms
// Distance of Earth to Moon: 384,400 km
case class GravitationalForce(
  bigG: Double = GravitationalForce.bigG
) extends ParticleFunction2 {
  def apply(
    p1: Particle,
    p2: Particle
  ): Vector3D = {
    // a vector from p1 to p2
    val p12: Vector3D = p2.p - p1.p

    val p12Norm = p12.normalize

    val p12Length = p12.len

    p12Norm * bigG * p1.m * p2.m / Math.pow(p12Length, 2)
  }
}

trait SpringTrait extends ParticleFunction2 {
  val springConstantTension: Double
  val springConstantCompression: Double
  val restLength: Double

  def apply(
    p1: Particle,
    p2: Particle
  ): Vector3D = {
    // a vector from p1 to p2
    val p12: Vector3D = p2.p - p1.p

    // the distance between the two particles
    val distance = p12.len

    // distance of the particles relative to the rest length of the spring
    val restDistance = distance - restLength

    val springConst = if (restDistance > 0) springConstantTension else springConstantCompression

    // normalize the vector between the two particles
    val p12Norm = p12.normalize

    p12Norm * springConst * restDistance
  }
}

object SpringFactory {
  implicit val playFormat: Format[SpringFactory] = new Format[SpringFactory] {
    override def reads(json: JsValue) = Json.reads[SpringFactory].reads(json)

    override def writes(f: SpringFactory): JsValue = Json.writes[SpringFactory].writes(f).deepMerge(
      Json.obj("type" -> JsString("Spring"))
    )
  }

  val DefaultSpringConstant: Double = 1.0

  def createForce(
    p1: Particle,
    p2: Particle,
    springConstantTension: Option[Double],
    springConstantCompression: Option[Double],
    restLength: Option[Double],
    maxForce: Option[Double]
  ): Force2 =
    Force2(
      id = Force.nextForceId,
      p1Id = p1.id,
      p2Id = p2.id,
      Spring(
        springConstantTension = springConstantTension.getOrElse(DefaultSpringConstant),
        springConstantCompression = springConstantCompression.getOrElse(DefaultSpringConstant),
        restLength = restLength.getOrElse(p1.distance(p2))
      ),
      maxForce = maxForce.getOrElse(Double.MaxValue)
    )
}

case class SpringFactory(
  pIds: List[List[Particle.ID]],
  springConstantTension: Option[Double],
  springConstantCompression: Option[Double],
  restLength: Option[Double],
  maxForce: Option[Double]
) extends Force2CombinationFactory(pIds) {
  override protected def createForces(
    p1: Particle,
    p2: Particle
  ): List[Force2] = List(
    SpringFactory.createForce(
      p1,
      p2,
      springConstantTension,
      springConstantCompression,
      restLength,
      maxForce
    )
  )
}

case class Spring(
  springConstantTension: Double,
  springConstantCompression: Double,
  restLength: Double
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
  protected def createForce(particle: Particle): Force1 =
    Force1(
      id = Force.nextForceId,
      pId = particle.id,
      ConstantForceParticleFunction1(forceVector.getOrElse(Vector3D()))
    )
}

case class ConstantForceParticleFunction1(
  forceVector: Vector3D
) extends ParticleFunction1 {
  def apply(p: Particle): Vector3D = forceVector
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
  forceFunc: Option[ParticleFunction1]
) extends Force1Factory(pIds) {

  protected def createForce(particle: Particle): Force1 =
    Force1(
      id = Force.nextForceId,
      particle.id,
      forceFunc.getOrElse(ConstantVector3DParticleFunction1(Vector3D(y = 1.0)))
    )
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
  flowFunc: Option[ParticleFunction1]
) extends Force1Factory(pIds) {
  override protected def createForce(particle: Particle): Force1 =
    Force1(
      id = Force.nextForceId,
      pId = particle.id,
      forceFunc = Drag(
        fluidDensity.getOrElse(0.5),
        dragCoeff.getOrElse(0.47),
        flowFunc.getOrElse(
          ConstantVector3DParticleFunction1(
            vector = Vector3D.ZeroValue
          )
        )
      )
    )
}

case class Drag(
  fluidDensity: Double = 0.5,
  dragCoeff: Double = 0.47,
  flowFunc: ParticleFunction1 = ConstantVector3DParticleFunction1(
    vector = Vector3D.ZeroValue
  )
) extends ParticleFunction1 {
  // m^2 / s^2 * kg / m^3 * m^2 = (kg * m) / s^2
  def apply(p: Particle): Vector3D = {
    val flow = flowFunc(p)
    val relativeFlow = flow - p.v
    val relativeSpeed = relativeFlow.len
    relativeFlow.normalize * (0.5 * fluidDensity * relativeSpeed * relativeSpeed * dragCoeff * p.area)
  }
}

trait DamperTrait extends ParticleFunction2 {
  val dampingCoeffTension: Double
  val dampingCoeffCompression: Double

  def apply(
    p1: Particle,
    p2: Particle
  ) = {
    // a vector from p1 to p2
    val p12: Vector3D = p2.p - p1.p

    // normalize the vector between the two particles
    val p12Norm = p12.normalize

    // get the scala projection of p1.v and p2.v onto p12Norm
    // i.e. the particles' velocities along the vector between the particles
    val a1: Double = p1.v.dotProduct(p12Norm)
    val a2: Double = p2.v.dotProduct(p12Norm)

    // calc the speed at which the particles are moving towards each other (positive/compression) or away (negative/tension)
    val closingSpeed = a1 - a2

    // determine if the particles are moving towards each other (compression) or away (tension)
    val isInCompression: Boolean = closingSpeed > 0.0

    // pick the correct damping coefficient
    val dampingCoeff = if (isInCompression) dampingCoeffCompression else dampingCoeffTension

    // calculate the damping force
    p12Norm * closingSpeed * -dampingCoeff
  }
}

object DamperFactory {
  implicit val playFormat: Format[DamperFactory] = new Format[DamperFactory] {
    override def reads(json: JsValue) = Json.reads[DamperFactory].reads(json)

    override def writes(f: DamperFactory): JsValue = Json.writes[DamperFactory].writes(f).deepMerge(
      Json.obj("type" -> JsString("Damper"))
    )
  }

  val DefaultDampingCoeff: Double = 20

  def createForce(
    p1: Particle,
    p2: Particle,
    dampingCoeffTension: Option[Double],
    dampingCoeffCompression: Option[Double],
    maxForce: Option[Double]
  ): Force2 =
    Force2(
      id = Force.nextForceId,
      p1.id,
      p2.id,
      forceFunc = Damper(
        dampingCoeffTension.getOrElse(DefaultDampingCoeff),
        dampingCoeffCompression.getOrElse(DefaultDampingCoeff)
      ),
      maxForce = maxForce.getOrElse(Double.MaxValue)
    )
}

case class DamperFactory(
  pIds: List[List[Particle.ID]],
  dampingCoeffTension: Option[Double],
  dampingCoeffCompression: Option[Double],
  maxForce: Option[Double]
) extends Force2CombinationFactory(pIds) {
  override protected def createForces(
    p1: Particle,
    p2: Particle
  ): List[Force2] = List(
    DamperFactory.createForce(
      p1,
      p2,
      dampingCoeffTension,
      dampingCoeffCompression,
      maxForce
    )
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
  * @param dampingCoeffTension the viscous damping coefficient when the damper is in tension
  * @param dampingCoeffCompression the viscous damping coefficient when the damper is in compression
  */
case class Damper(
  dampingCoeffTension: Double,
  dampingCoeffCompression: Double
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
  springConstTension: Option[Double],
  springConstCompression: Option[Double],
  restLength: Option[Double],
  dampingCoeffTension: Option[Double],
  dampingCoeffCompression: Option[Double],
  maxSpringForce: Option[Double],
  maxDamperForce: Option[Double]
) extends Force2CombinationFactory(pIds) {
  def criticalDampingCoeffTension(mass: Double): Double =
    2.0 * Math.sqrt(springConstTension.getOrElse(SpringFactory.DefaultSpringConstant) * mass)

  def criticalDampingCoeffCompression(mass: Double): Double =
    2.0 * Math.sqrt(springConstCompression.getOrElse(SpringFactory.DefaultSpringConstant) * mass)

  override protected def createForces(
    p1: Particle,
    p2: Particle
  ): List[Force2] = List(
    SpringFactory.createForce(
      p1,
      p2,
      springConstTension,
      springConstCompression,
      restLength,
      maxForce = maxSpringForce
    ),
    DamperFactory.createForce(
      p1,
      p2,
      dampingCoeffTension = dampingCoeffTension.orElse[Double](Some(criticalDampingCoeffTension(p2.m))),
      dampingCoeffCompression = dampingCoeffCompression.orElse[Double](Some(criticalDampingCoeffCompression(p2.m))),
      maxForce = maxDamperForce
    )
  )
}

