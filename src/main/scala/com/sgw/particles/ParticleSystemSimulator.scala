package com.sgw.particles

import scala.swing.{MainFrame, SimpleSwingApplication}
import com.sgw.particles.factories.{SimpleParticleSystemFactory, PlanetaryParticleSystemFactory, BeamParticleSystemFactory}
import com.sgw.particles.utils.Loggable

/**
 * author: Steve Wampler
 */
object ParticleSystemSimulator extends SimpleSwingApplication with Loggable {
  var psargs: ParticleSystemArgs = null // yeah, hack, might want to use an Option here

  def top = new MainFrame {
    title = "Particles"
    contents = ParticleSystemView(
      psargs.config.getString("type") match {
        case Some(SimpleParticleSystemFactory.name)    => SimpleParticleSystemFactory(psargs.config)
        case Some(BeamParticleSystemFactory.name)      => BeamParticleSystemFactory(psargs.config)
        case Some(PlanetaryParticleSystemFactory.name) => PlanetaryParticleSystemFactory(psargs.config)
        case _ => throw new RuntimeException("Unknown particle system type: " + psargs.config.getString("type"))
      },
      psargs.config.getDouble("dt").getOrElse(0.01),
      psargs.config.getDouble("sleep").getOrElse(0.01)
    )
  }

  override def startup(args: Array[String]) {
    psargs = ParticleSystemArgs(args)
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
