package com.sgw.particles

import java.awt.EventQueue

case class Simulation(updatable: Updatable, dt: Double, sleepTime: Double) {
  val simThread = new Thread("sim") {
    var paused = false

    def togglePaused = this.synchronized {
      if (paused) resumeSim else pauseSim
    }

    def pauseSim = this.synchronized { paused = true }

    def resumeSim = this.synchronized {
      paused = false
      notify()
    }

    override def run() {
      while (true) {
        // TODO: this somehow belongs in the ParticleSystemSimulationView
        EventQueue.invokeLater(runnable)

        try {
          Thread.sleep((sleepTime * 1000L).toLong)

          this.synchronized {
            while (paused) wait
          }
        } catch {
          case e: InterruptedException =>
        }
      }
    }
  }

  val runnable = new Runnable() {
    var t = 0.0
    def run() {
      updatable.beginUpdate(t)
      updatable.update(t)
      updatable.endUpdate(t)
      t = t + dt
    }
  }

  def start = simThread.start()

  def togglePaused = simThread.togglePaused
}
