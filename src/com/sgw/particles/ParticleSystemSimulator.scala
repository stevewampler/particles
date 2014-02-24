package com.sgw.particles

import scala.swing.{MainFrame, SimpleSwingApplication}
import com.sgw.particles.factories.{PlanetaryParticleSystemFactory, StringParticleSystemFactory, BeamParticleSystemFactory}

object ParticleSystemSimulator extends SimpleSwingApplication {
  val psType = 1

  val particleSystem = psType match {
    case 0 => StringParticleSystemFactory.createParticleSystem(
      numParticles = 6,
      spread = 30,
      force1Factories =
        GravityFactory() ::
          DragFactory(
            flowFunc = ParticlePositionFunction(
              CompositeVector3DFunction(
                XYNormalVector3DFunction :: ScaleVector3DFunction(0.5) :: Nil
              )
            )
          ) ::
          DragFactory(
            flowFunc = ParticleTimeFunction(
              SinWaveVector3DFunction(
                offset     = Vector3D(0.0), // m/s
                amplitude  = Vector3D(50.0), // m/s
                frequency  = Vector3D(0.25, 1.0, 1.0), // cycles/sec
                phase      = Vector3D() // radians
              )
            )
          ) ::
          Nil,
      force2Factories =
        SpringDamperFactory(5, 10, 1, maxForce = 500) ::
          Nil
    )
    case 1 => {
      val i = 5
      def anchorFilter(row: Integer, numRows: Integer, col: Integer, numCols: Integer) =  col == 0 || col == numCols - 1
      def particleFilter(row: Integer, numRows: Integer, col: Integer, numCols: Integer) = true
      def maxParticleAgeFunc(row: Integer, numRows: Integer, col: Integer, numCols: Integer) = Double.MaxValue // Math.random() * 60
      def maxForceAgeFunc(row1: Integer, row2: Integer, numRows: Integer, col1: Integer, col2: Integer, numCols: Integer) = Double.MaxValue
      BeamParticleSystemFactory.createParticleSystem(
        numRows = 3,
        numCols = 10,
        spread  = 30,
        m = 10,
        anchorFilter = anchorFilter,
        particleFilter = particleFilter,
        maxParticleAgeFunc = maxParticleAgeFunc,
        springConst  = Array( 500, 5000, 2000, 1000, 1000,  500)(i),
        dampingCoeff = Array( 150,  200,  500,  500,  200,    5)(i),
        maxForce     = Array(1000, 3000, 1000, 1000,  900, 1500)(i),
        maxSpringAgeFunc = maxForceAgeFunc
      )
    }
    case 2 => {
      val i = 1
      def anchorFilter(row: Integer, numRows: Integer, col: Integer, numCols: Integer) =  row == 0 && (col == 0 || col == numCols - 1)
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
  }

  val dt = psType match {
    case 0 => 0.01
    case 1 => 0.01
    case 2 => 0.01
    case 3 => 60 * 60
  }

  val sleepTime = psType match {
    case 0 => 0.01
    case 1 => 0.01
    case 2 => 0.01
    case 3 => 0.001
  }

  val simulation = Simulation(particleSystem, dt, sleepTime)

  val particleSystemSimulationView = ParticleSystemSimulationView(simulation, particleSystem)

  def top = new MainFrame {
    title = "Particles"
    contents = particleSystemSimulationView
  }

  simulation.start
}
