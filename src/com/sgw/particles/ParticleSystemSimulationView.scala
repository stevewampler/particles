package com.sgw.particles

import scala.swing.Swing._
import scala.swing.Panel
import java.awt.{Color, Graphics2D}
import scala.swing.event.{MouseReleased, MousePressed}

case class ParticleSystemSimulationView(simulation: Simulation, particleSystem: ParticleSystem) extends Panel {
  val modelBounds = particleSystem.bounds
  val width = 800
  val height = 800
  var renderers = (particleSystem.particles ++ particleSystem.forces).map(obj => Renderer(obj))

  def onUpdated(particleSystem: ParticleSystem, t: Double) = repaint()

  particleSystem.onUpdatedListener = onUpdated

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
      simulation.togglePaused
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
    renderers = renderers.filter(!_.dead)
    val width = g.getClipBounds.width
    val height = g.getClipBounds.height
    val scale = RendererUtils.calcScale(width, height, modelBounds)
    renderers.foreach(renderer => renderer.render(g, modelBounds, width, height, scale))
  }
}
