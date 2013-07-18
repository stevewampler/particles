package com.sgw.particles.factories

import com.sgw.particles._
import com.sgw.particles.Particle
import com.sgw.particles.SpringDamper
import com.sgw.particles.Gravity

/**
 * author: steve
 */
object BeamParticleSystemFactory {
  def defaultAnchorFilter(row: Integer, numRows: Integer, col: Integer, numCols: Integer) = (row == 0 && (col == 0 || col == numCols - 1))

  def defaultParticleFilter(row: Integer, numRows: Integer, col: Integer, numCols: Integer) = col >= row && (numCols - col - 1) >= row

  def createParticleSystem(
      numRows: Int,
      numCols: Int,
      spread: Int = 30,
      m: Double = 10,
      anchorFilter: (Integer, Integer, Integer, Integer) => Boolean = defaultAnchorFilter,
      particleFilter: (Integer, Integer, Integer, Integer) => Boolean = defaultParticleFilter,
      springConst: Double = 500,
      dampingCoeff: Double = 100.0,
      maxForce: Double = 500) = {
    val particleMatrix: Seq[Seq[Particle]] = (0 until numRows).map(
      row => {
        (0 until numCols).map(
          col => {
            val x = -(numCols / 2) * spread + col * spread
            val y = -(numRows / 2) * spread + row * spread
            val mass = if (anchorFilter(row, numRows, col, numCols)) Double.MaxValue else if (particleFilter(row, numRows, col, numCols)) m else 0.0
            Particle(mass, p = Vector3D(x, y, 0.0), radius=m, area = 1)
          }
        )
      }
    )

    val gravityForces: Seq[Force] = particleMatrix.flatten.filter(particle => particle.m != Double.MaxValue).map(particle => Gravity(particle))

    val rowSprings: Seq[Force] = particleMatrix.map(
      rowParticles => rowParticles.filter(_.m > 0.0).init.zip(rowParticles.filter(_.m > 0.0).tail).map(pair => SpringDamper(pair._1, pair._2, springConst, spread, dampingCoeff, maxForce))
    ).flatten

    val colSprings: Seq[Force] = (0 until numCols).map(
      col => {
        (0 until numRows - 1).filter(row => particleMatrix(row)(col).m > 0.0 && particleMatrix(row+1)(col).m > 0.0).map(
          row => {
            SpringDamper(particleMatrix(row)(col), particleMatrix(row+1)(col), springConst, spread, dampingCoeff, maxForce)
          }
        )
      }
    ).flatten

    val r = spread / Math.cos(Math.PI/4)

    val crossSprings1: Seq[Force] = (0 until numRows - 1).map(
      row => {
        (0 until numCols - 1).filter(col => particleMatrix(row)(col).m > 0.0 && particleMatrix(row+1)(col+1).m > 0.0).map(
          col => {
            SpringDamper(particleMatrix(row)(col), particleMatrix(row+1)(col+1), springConst, r, dampingCoeff, maxForce)
          }
        )
      }
    ).flatten

    val crossSprings2: Seq[Force] = (0 until numRows - 1).map(
      row => {
        (0 until numCols - 1).filter(col => particleMatrix(row)(col+1).m > 0.0 && particleMatrix(row+1)(col).m > 0.0).map(
          col => {
            SpringDamper(particleMatrix(row)(col+1), particleMatrix(row+1)(col), springConst, r, dampingCoeff, maxForce)
          }
        )
      }
    ).flatten
    /*
    val wind: Seq[Force] = (0 until numRows).map(
      row => {
        (0 until numCols).filter(col => particleMatrix(row)(col).m > 0.0).map(
          col => {
            Drag(particleMatrix(row)(col), flowFunc = SinWaveParticleFunction())
          }
        )
      }
    ).flatten
    */
    val particles = particleMatrix.flatten.filter(_.m > 0.0)
    val forces = gravityForces ++ rowSprings ++ colSprings ++ crossSprings1 ++ crossSprings2 // ++ wind
    val bounds = ParticleSystemUtils.bounds(particles) * 2

    ParticleSystem(
      particles,
      forces,
      bounds
    )
  }
}
