package com.sgw.particles.swing

import java.awt.{Color, EventQueue, Graphics2D}

import com.sgw.particles.model.ParticleSystemFactory
import play.api.libs.json.{Format, Json}

import scala.swing.Panel
import scala.swing.Swing._
import scala.swing.event.{MousePressed, MouseReleased}

object ParticleSystemView {
  implicit def playFormat: Format[ParticleSystemView] = Json.format[ParticleSystemView]
}

case class ParticleSystemView(
  particleSystemFactory: ParticleSystemFactory,
  dt: Option[Double],
  sleep: Option[Double]
) extends Panel {
  private var pSys = particleSystemFactory.createParticleSystem.initForces
  private val width = 800
  private val height = 800
  private val _dt = dt.getOrElse(0.01)
  private val _sleep = sleep.getOrElse(0.0)

  def particleSystemName: String = pSys.name

  background = Color.white
  preferredSize = (width, height)
  focusable = true

  listenTo(mouse.clicks)

  reactions += {
    case e: MousePressed  => onMousePressed(e)
    case e: MouseReleased => onMouseReleased(e)
  }

  private def onMousePressed(e: MousePressed): Unit = {
    println("onMousePressed: " + e)
    requestFocusInWindow()
    if (e.triggersPopup) {
      println("popup1")
    } else {
      simThread.togglePaused
    }
  }

  private def onMouseReleased(e: MouseReleased): Unit = {
    println("onMouseReleased: " + e)
    requestFocusInWindow()
    if (e.triggersPopup) {
      println("popup2")
    } else {
      // simThread.togglePaused
    }
  }

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)

    pSys.forceMap.values.foreach { force =>
      ForceRenderer.render(
        g,
        pSys,
        force
      )
    }

    pSys.particleMap.values.foreach { particle =>
      ParticleRenderer.render(
        g,
        pSys,
        particle
      )
    }
  }

  private val runnable = new Runnable() {
    def run() {
      pSys = pSys(_dt)

      repaint()
    }
  }

  private val simThread = new Thread("sim") {
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
        EventQueue.invokeAndWait(runnable)

        try {
          Thread.sleep((_sleep * 1000L).toLong)

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
