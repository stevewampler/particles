package com.sgw.particles

import scala.swing.{MainFrame, SimpleSwingApplication}
import com.sgw.particles.factories.{PlanetaryParticleSystemFactory, StringParticleSystemFactory, BeamParticleSystemFactory}

/**
 * author: Steve Wampler
 */
object ParticleSystemSimulator extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Particles"
    val psType = 1
    contents = ParticleSystemView(
      psType match {
        case 0 => StringParticleSystemFactory.createParticleSystem(numParticles = 6, spread = 30)
        case 1 => {
          val i = 5
          def anchorFilter(row: Integer, numRows: Integer, col: Integer, numCols: Integer) =  ((col == 0 || col == numCols - 1))
          def particleFilter(row: Integer, numRows: Integer, col: Integer, numCols: Integer) = true
          BeamParticleSystemFactory.createParticleSystem(
            numRows = 3,
            numCols = 10,
            spread  = 30,
            anchorFilter = anchorFilter,
            particleFilter = particleFilter,
            springConst  = Array( 500, 5000, 2000, 1000, 1000,  500)(i),
            dampingCoeff = Array( 150,  200,  500,  500,  200,    5)(i),
            maxForce     = Array(1000, 3000, 1000, 1000,  900, 1500)(i)
          )
        }
        case 2 => {
          val i = 1
          def anchorFilter(row: Integer, numRows: Integer, col: Integer, numCols: Integer) =  (row == 0 && (col == 0 || col == numCols - 1))
          def particleFilter(row: Integer, numRows: Integer, col: Integer, numCols: Integer) = col >= row && (numCols - col - 1) >= row
          BeamParticleSystemFactory.createParticleSystem(
            numRows = 3,
            numCols = 10,
            spread  = 30,
            anchorFilter = anchorFilter,
            particleFilter = particleFilter,
            springConst  = Array(1000, 5000, 2000, 1000, 1000)(i),
            dampingCoeff = Array( 200,  200,  500,  500,  200)(i),
            maxForce     = Array(10000, 3000, 1000, 1000,  900)(i)
          )
        }
        case 3 => {
          PlanetaryParticleSystemFactory.createParticleSystem()
        }
      },
      dt = psType match {
        case 0 => 0.01
        case 1 => 0.01
        case 2 => 0.01
        case 3 => 60 * 60
      },
      sleep = psType match {
        case 0 => 0.01
        case 1 => 0.01
        case 2 => 0.01
        case 3 => 0.001
      }
    )
  }
}
