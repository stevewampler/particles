package com.sgw.particles.swing

import java.awt.{Color, EventQueue, Graphics2D}

import com.sgw.particles.model.ParticleSystem

import scala.swing.Panel
import scala.swing.Swing._
import scala.swing.event.{MousePressed, MouseReleased}

case class ParticleSystemView(particleSystem: ParticleSystem) extends Panel {
  val modelBounds = particleSystem.bounds
  val width = 800
  val height = 800

  background = Color.white
  preferredSize = (width, height)
  focusable = true

  listenTo(mouse.clicks)

  reactions += {
    case e: MousePressed  => onMousePressed(e)
    case e: MouseReleased => onMouseReleased(e)
  }

  def onMousePressed(e: MousePressed) = {
    println("onMousePressed: " + e)
    requestFocusInWindow()
    if (e.triggersPopup) {
      println("popup1")
    } else {
      simThread.togglePaused
    }
  }

  def onMouseReleased(e: MouseReleased) = {
    println("onMouseReleased: " + e)
    requestFocusInWindow()
    if (e.triggersPopup) {
      println("popup2")
    } else {
      // simThread.togglePaused
    }
  }

  override def paintComponent(g: Graphics2D) = {
    super.paintComponent(g)
    renderers.foreach(renderer => renderer.render(g, modelBounds))
  }

  val renderers = (particleSystem.particles ++ particleSystem.forces).map(obj => Renderer(obj))

  val runnable = new Runnable() {
    var t = 0.0
    def run() {
      particleSystem.particles.foreach(particle => {
        particle.next
      })
      particleSystem.forces.foreach(force => {
        force.apply
      })
      particleSystem.particles.foreach(particle => {
        particle(t)
      })
      repaint()
      t = t + particleSystem.dt
    }
  }

  val simThread = new Thread("sim") {
    var paused = false

    def togglePaused() = this.synchronized {
      if (paused)
        resumeSim()
      else
        pauseSim()
    }

    def pauseSim() = this.synchronized {
      paused = true
    }

    def resumeSim() = this.synchronized {
      paused = false
      notify()
    }

    override def run() {
      while (true) {
        EventQueue.invokeLater(runnable)

        try {
          Thread.sleep((particleSystem.sleep * 1000L).toLong)

          this.synchronized {
            while (paused)
              wait
          }
        } catch {
          case e: InterruptedException =>
        }
      }
    }
  }

  simThread.start()
}
