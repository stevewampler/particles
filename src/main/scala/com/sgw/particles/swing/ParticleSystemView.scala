package com.sgw.particles.swing

import java.awt.{Color, EventQueue, Graphics2D}

import com.sgw.particles.model.ParticleSystem

import scala.swing.Panel
import scala.swing.Swing._
import scala.swing.event.{MousePressed, MouseReleased}

case class ParticleSystemView(initialParticleSystem: ParticleSystem) extends Panel {
  private var pSys = initialParticleSystem
  private val modelBounds = initialParticleSystem.bounds
  private val width = 800
  private val height = 800

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

  val runnable = new Runnable() {
    def run() {
      pSys = pSys(pSys.dt)

      repaint()
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
//        EventQueue.invokeLater(runnable)

//        val startTime = System.currentTimeMillis()
        EventQueue.invokeAndWait(runnable)
//        val endTime = System.currentTimeMillis()

//        println(endTime - startTime)

        try {
          Thread.sleep((pSys.sleep * 1000L).toLong)

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
