package com.sgw.particles.swing

import com.sgw.particles.model.ParticleSystemFactory
import com.sgw.particles.utils.Loggable
import play.api.libs.json.{JsError, JsSuccess, Json}
import twitter.scalding.Args

import scala.io.Source
import scala.swing.{MainFrame, SimpleSwingApplication}

object ParticleSystemSimulator extends SimpleSwingApplication with Loggable {
  private var maybeParticleSystemView: Option[ParticleSystemView] = None

  def top = maybeParticleSystemView.map { particleSystemView =>
    new MainFrame() {
      title = particleSystemView.particleSystemName
      contents = particleSystemView
    }
  }.getOrElse {
    throw new RuntimeException(
      "No particle system view."
    )
  }

  override def startup(args: Array[String]) {
    val psargs = Args(args)

    maybeParticleSystemView = psargs.getURI("file").map { uri =>
      if (uri.isAbsolute)
        uri.toURL
      else
        getClass.getResource(uri.getPath)
    }.map(Source.fromURL).map { source =>
      source.getLines().mkString("")
    }.map { json =>
      Json.fromJson[ParticleSystemView](Json.parse(json)) match {
        case JsSuccess(particleSystemView, _) => particleSystemView
        case JsError(errors) =>
          val errorStrings = errors.map { case (path, validationErrors) =>
            List(
              s"  ${path.toString()}:",
              validationErrors.map(_.toString).mkString("    ", "\n    ", "")
            ).mkString("\n")
          }
          throw new RuntimeException(s"Failed to read a particle system view.\n$errorStrings")
      }
    }

    super.startup(args)
  }
}

//        case 1 => { // beam
//          val i = 0
//          def anchorFilter(row: Integer, numRows: Integer, col: Integer, numCols: Integer) =  ((col == 0 || col == numCols - 1))
//          def particleFilter(row: Integer, numRows: Integer, col: Integer, numCols: Integer) = true
//          BeamParticleSystemFactory.createParticleSystem(
//            numRows = 3,
//            numCols = 10,
//            spread  = 30,
//            anchorFilter = anchorFilter,
//            particleFilter = particleFilter,
//            springConst  = Array( 500, 5000, 2000, 1000, 1000)(i),
//            dampingCoeff = Array( 150,  200,  500,  500,  200)(i),
//            maxForce     = Array(1000, 3000, 1000, 1000,  900)(i)
//          )
//        }
//        case 2 => { // bridge
//          val i = 1
//          def anchorFilter(row: Integer, numRows: Integer, col: Integer, numCols: Integer) =  (row == 0 && (col == 0 || col == numCols - 1))
//          def particleFilter(row: Integer, numRows: Integer, col: Integer, numCols: Integer) = col >= row && (numCols - col - 1) >= row
//          BeamParticleSystemFactory.createParticleSystem(
//            numRows = 3,
//            numCols = 10,
//            spread  = 30,
//            anchorFilter = anchorFilter,
//            particleFilter = particleFilter,
//            springConst  = Array(1000, 5000, 2000, 1000, 1000)(i),
//            dampingCoeff = Array( 200,  200,  500,  500,  200)(i),
//            maxForce     = Array(10000, 3000, 1000, 1000,  900)(i)
//          )
//        }
//        case 3 => { // planets
//          PlanetaryParticleSystemFactory.createParticleSystem()
//        }
