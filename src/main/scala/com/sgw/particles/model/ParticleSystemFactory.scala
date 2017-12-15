package com.sgw.particles.model

import play.api.libs.json._

object ParticleSystemFactory {
  implicit def playFormat: Format[ParticleSystemFactory] = new Format[ParticleSystemFactory] {

    override def reads(json: JsValue): JsResult[ParticleSystemFactory] =
      (json \ "type")
        .validate[String]
        .flatMap {
          case "SimpleParticleSystemFactory"    => SimpleParticleSystemFactory.playFormat.reads(json)
          case "BeamParticleSystemFactory"      => BeamParticleSystemFactory.playFormat.reads(json)
          case "PlanetaryParticleSystemFactory" => PlanetaryParticleSystemFactory.playFormat.reads(json)
        }

    override def writes(func: ParticleSystemFactory): JsValue = {
      func match {
        case factory: SimpleParticleSystemFactory => SimpleParticleSystemFactory.playFormat.writes(factory)
        case factory: BeamParticleSystemFactory   => BeamParticleSystemFactory.playFormat.writes(factory)
        case factory: PlanetaryParticleSystemFactory => PlanetaryParticleSystemFactory.playFormat.writes(factory)
      }
    }
  }
}

sealed trait ParticleSystemFactory {
  def createParticleSystem: ParticleSystem
}

// SimpleParticleSystemFactory

object SimpleParticleSystemFactory {
  implicit val playFormat: Format[SimpleParticleSystemFactory] = new Format[SimpleParticleSystemFactory] {
    override def reads(json: JsValue) = Json.reads[SimpleParticleSystemFactory].reads(json)

    override def writes(factory: SimpleParticleSystemFactory): JsValue = Json.writes[SimpleParticleSystemFactory].writes(factory).deepMerge(
      Json.obj("type" -> JsString(factory.getClass.getName))
    )
  }
}

case class SimpleParticleSystemFactory(
  name: String,
  particles: List[ParticleFactory],
  forces: List[ForceFactory],
  bounds: Option[Bounds3D]
) extends ParticleSystemFactory {

  def createParticleSystem: ParticleSystem =
    ParticleSystem.createFromFactories(
      name = name,
      particleFactories = particles,
      forceFactories = forces,
      maybeBounds = bounds
    )
}

// BeamParticleSystemFactory

object BeamParticleSystemFactory {
  implicit val playFormat: Format[BeamParticleSystemFactory] = new Format[BeamParticleSystemFactory] {
    override def reads(json: JsValue) = Json.reads[BeamParticleSystemFactory].reads(json)

    override def writes(factory: BeamParticleSystemFactory): JsValue = Json.writes[BeamParticleSystemFactory].writes(factory).deepMerge(
      Json.obj("type" -> JsString(factory.getClass.getName))
    )
  }
}

case class BeamParticleSystemFactory(
  name: Option[String],
  numRows: Option[Int],
  numCols: Option[Int],
  spread: Option[Double],
  m: Option[Double],
  springConst: Option[Double],
  dampingCoeff: Option[Double],
  maxForce: Option[Double],
  fluidDensity: Option[Double],
  dragCoeff: Option[Double]
) extends ParticleSystemFactory {
  private def anchorFilter(
    row: Integer,
    numRows: Integer,
    col: Integer,
    numCols: Integer
  ) = row == 0 && (col == 0 || col == numCols - 1)

  def particleFilter(
    row: Integer,
    numRows: Integer,
    col: Integer,
    numCols: Integer
  ) = col >= row && (numCols - col - 1) >= row

  override def createParticleSystem: ParticleSystem =
    createParticleSystem(
      name = name.getOrElse("Beam"),
      numRows = numRows.getOrElse(4),
      numCols = numCols.getOrElse(10),
      spread = spread.getOrElse(100.0),
      m = m.getOrElse(1.0),
      springConst = springConst.getOrElse(100.0),
      dampingCoeff = dampingCoeff.getOrElse(20.0),
      maxForce = maxForce.getOrElse(10000),
      fluidDensity = fluidDensity.getOrElse(0.5),
      dragCoeff = dragCoeff.getOrElse(0.47)
    )

  private def createParticleSystem(
    name: String,
    numRows: Int,
    numCols: Int,
    spread: Double,
    m: Double,
    springConst: Double = 500.0,
    dampingCoeff: Double = 100.0,
    maxForce: Double = 500,
    fluidDensity: Double = 0.5,
    dragCoeff: Double = 0.47
  ): ParticleSystem = {
    val particleMatrix: Seq[Seq[Particle]] =
      (0 until numRows).map { row =>
        (0 until numCols).map { col =>
          val id = row * numCols + col
          val x = -(numCols / 2) * spread + col * spread
          val y = -(numRows / 2) * spread + row * spread
          val mass = if (anchorFilter(row, numRows, col, numCols)) Double.MaxValue
          else if (particleFilter(row, numRows, col, numCols)) m
          else 0.0

          Particle(
            id = id,
            name = id.toString,
            t = 0.0,
            m = mass,
            p = Vector3D(x, y),
            v = Vector3D.ZeroValue,
            a = Vector3D.ZeroValue,
            f = Vector3D.ZeroValue,
            m1 = mass,
            f1 = Vector3D.ZeroValue,
            radius = mass,
            age = 0.0,
            area = mass
          )
        }
      }

    val particleMap = particleMatrix.flatten.map { particle =>
      particle.id -> particle
    }.toMap

    val gravityForces: List[Force] = {
      val pIds = particleMatrix.flatten.filter { p =>
        p.m != Double.MaxValue && p.m > 0.0
      }.map { p =>
        p.id
      }.toList

      GravityFactory(pIds).createForces(particleMap)
    }

    val rowSprings: Seq[Force] = {
      val listOfListOfParticleIds: List[List[Particle.ID]] =
        particleMatrix.flatMap { rowParticles =>
          rowParticles.filter { particle =>
            particle.m > 0.0
          }.init.zip(
            rowParticles.filter { particle =>
              particle.m > 0.0
            }.tail
          ).map { case (p1, p2) =>
            List(p1.id, p2.id)
          }
        }.toList

      SpringDamperFactory(
        listOfListOfParticleIds,
        springConstant = Some(springConst),
        restLength = None,
        dampingCoeff = Some(dampingCoeff),
        maxSpringForce = Some(maxForce),
        maxDamperForce = None
      ).createForces(particleMap)
    }

    val colSprings: Seq[Force] = {
      val listOfListOfParticleIds: List[List[Particle.ID]] = (0 until numCols).flatMap { col =>
        (0 until numRows - 1).filter { row =>
          particleMatrix(row)(col).m > 0.0 && particleMatrix(row + 1)(col).m > 0.0
        }.map { row =>
          List(
            particleMatrix(row)(col).id,
            particleMatrix(row+1)(col).id
          )
        }
      }.toList

      SpringDamperFactory(
        listOfListOfParticleIds,
        Some(springConst),
        Some(spread),
        Some(dampingCoeff),
        maxSpringForce = Some(maxForce),
        maxDamperForce = None
      ).createForces(particleMap)
    }

    val r = spread / Math.cos(Math.PI/4)

    val crossSprings1: Seq[Force] = {
      val listOfListOfParticleIds: List[List[Particle.ID]] = (0 until numRows - 1).flatMap { row =>
        (0 until numCols - 1).filter { col =>
          particleMatrix(row)(col).m > 0.0 && particleMatrix(row + 1)(col + 1).m > 0.0
        }.map { col =>
          List(
            particleMatrix(row)(col).id,
            particleMatrix(row+1)(col+1).id
          )
        }
      }.toList

      SpringDamperFactory(
        pIds = listOfListOfParticleIds,
        springConstant = Some(springConst),
        restLength = Some(r),
        dampingCoeff = Some(dampingCoeff),
        maxSpringForce = Some(maxForce),
        maxDamperForce = None
      ).createForces(particleMap)
    }

    val crossSprings2: Seq[Force] = {
      val listOfListOfParticleIds: List[List[Particle.ID]] = (0 until numRows - 1).flatMap { row =>
        (0 until numCols - 1).filter { col =>
          particleMatrix(row)(col + 1).m > 0.0 && particleMatrix(row + 1)(col).m > 0.0
        }.map { col =>
          List(
            particleMatrix(row)(col+1).id,
            particleMatrix(row+1)(col).id
          )
        }
      }.toList

      SpringDamperFactory(
        pIds = listOfListOfParticleIds,
        springConstant = Some(springConst),
        restLength = Some(r),
        dampingCoeff = Some(dampingCoeff),
        maxSpringForce = Some(maxForce),
        maxDamperForce = None
      ).createForces(particleMap)
    }

    val wind: Seq[Force] = {
      val pIds: List[Particle.ID] = (0 until numRows).flatMap { row =>
        (0 until numCols).filter { col =>
          particleMatrix(row)(col).m > 0.0
        }.map { col =>
          particleMatrix(row)(col).id
        }
      }.toList

      DragFactory(
        pIds,
        fluidDensity = Some(fluidDensity),
        dragCoeff = Some(dragCoeff),
        flowFunc = Some(
          ParticleTimeFunction(
            SinWaveVector3DFunction(
              offset  = Vector3D(10.0), // m/s
              amplitude = Vector3D(20.0, 1.0), // m/s
              frequency  = Vector3D(0.01, 1.0, 1.0), // cycles/sec
              phase = Vector3D()
            )
          )
        )
      ).createForces(particleMap)
    }

    val particles = particleMatrix.flatten.filter(_.m > 0.0).toList
    val forces = gravityForces ++ rowSprings ++ colSprings ++ crossSprings1 ++ crossSprings2 ++ wind
    val bounds = ParticleSystemUtils.bounds(particles) * 2

    ParticleSystem(
      name = name,
      particles,
      forces,
      Some(bounds)
    )
  }
}

object PlanetaryParticleSystemFactory {
  implicit val playFormat: Format[PlanetaryParticleSystemFactory] = new Format[PlanetaryParticleSystemFactory] {
    override def reads(json: JsValue) = Json.reads[PlanetaryParticleSystemFactory].reads(json)

    override def writes(factory: PlanetaryParticleSystemFactory): JsValue = Json.writes[PlanetaryParticleSystemFactory].writes(factory).deepMerge(
      Json.obj("type" -> JsString(factory.getClass.getName))
    )
  }
}

// PlanetaryParticleSystemFactory

// Mass of Moon: 7.34767309E22 kilograms
// Distance of Earth to Moon: 384,400 km
case class PlanetaryParticleSystemFactory(
  name: String
) extends ParticleSystemFactory {
  private def createParticle(
    id: Particle.ID,
    name: String,
    m: Double,
    p: Vector3D = Vector3D.ZeroValue,
    v: Vector3D = Vector3D.ZeroValue,
    radius: Double
  ): Particle = Particle(
    id = id,
    name = name,
    t = 0.0,
    m = m,
    p = p,
    v = v,
    a = Vector3D.ZeroValue,
    f = Vector3D.ZeroValue,
    m1 = m,
    f1 = Vector3D.ZeroValue,
    radius = radius,
    age = 0.0,
    area = 1.0
  )

  def createParticleSystem = {
    val sun     = createParticle(id = 0, name = "Sun",     m = 1.9890E30, radius=6.955E8)
    val mercury = createParticle(id = 1, name = "Mercury", m = 0.3301E24, p = Vector3D(0.460E11), v = Vector3D(0.0, -5.898E4), radius = 2.440E6)
    val venus   = createParticle(id = 2, name = "Venus",   m = 4.8676E24, p = Vector3D(1.075E11), v = Vector3D(0.0, -3.502E4), radius = 3.396E6)
    val earth   = createParticle(id = 3, name = "Earth",   m = 5.9726E24, p = Vector3D(1.496E11), v = Vector3D(0.0, -2.978E4), radius = 6.371E6)
    val mars    = createParticle(id = 4, name = "Mars",    m = 0.6417E24, p = Vector3D(2.279E11), v = Vector3D(0.0, -2.413E4), radius = 3.396E6)
    val jupiter = createParticle(id = 5, name = "Jupiter", m = 1898.3E24, p = Vector3D(7.405E11), v = Vector3D(0.0, -1.372E4), radius = 7.149E7)
    val saturn  = createParticle(id = 6, name = "Saturn",  m = 568.36E24, p = Vector3D(1.353E12), v = Vector3D(0.0, -1.018E4), radius = 6.027E7)
    val uranus  = createParticle(id = 7, name = "Uranus",  m = 86.816E24, p = Vector3D(2.741E12), v = Vector3D(0.0, -7.110E3), radius = 2.556E7)
    val neptune = createParticle(id = 8, name = "Neptune", m = 102.42E24, p = Vector3D(4.445E12), v = Vector3D(0.0, -5.500E3), radius = 2.476E7)
//    val pluto   = Particle(m = 0.6417E24, p = Vector3D(2.279E11), v = Vector3D(0.0, -2.413E4), radius = 3.396E6)

    val planets = mercury :: venus :: earth :: mars :: jupiter :: saturn :: uranus :: neptune :: Nil

    val solarSystem = sun :: planets

    val particleMap = solarSystem.map { particle =>
      particle.id -> particle
    }.toMap

    val listOfListOfParticleIds: List[List[Particle.ID]] = planets.map { planet =>
      List(sun.id, planet.id)
    }

    val forces = GravitationalForceFactory(
      listOfListOfParticleIds
    ).createForces(particleMap)

    val bounds = ParticleSystemUtils.bounds(solarSystem) * 2.1

    ParticleSystem(
      name = name,
      solarSystem,
      forces,
      Some(bounds)
    )
  }
}
