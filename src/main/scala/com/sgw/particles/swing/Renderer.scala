package com.sgw.particles.swing

import java.awt.geom.{Ellipse2D, Line2D}
import java.awt.{Color, Graphics2D}

import com.sgw.particles.model._
import com.sgw.particles.swing.RendererUtils.getForceColor

sealed trait Renderer[T] {
  def render(g: Graphics2D, pSys: ParticleSystem, item: T): Unit
}

case object ForceRenderer extends Renderer[Force] {
  def render(g: Graphics2D, pSys: ParticleSystem, force: Force): Unit = force match {
    case force1: Force1 => Force1Renderer.render(g, pSys, force1)
    case force2: Force2 => Force2Renderer.render(g, pSys, force2)
  }
}

case object ParticleRenderer extends Renderer[Particle] {
  def render(g: Graphics2D, pSys: ParticleSystem, particle: Particle) = {
    val width = g.getClipBounds.width
    val height = g.getClipBounds.height
    val scale = RendererUtils.calcScale(width, height, pSys.bounds)
    val size = (particle.radius * 2 * scale).toInt.min(20).max(2)
    g.setColor(Color.black)
    g.fill(
      new Ellipse2D.Double(
        particle.p.x * scale + width / 2.0 - size / 2.0,
        -particle.p.y * scale + height / 2.0 - size / 2.0,
        size,
        size
      )
    )
//    g.setColor(Color.blue)
//    println(s"particle.f = ${particle.f} ${particle.f1}")
//    RendererUtils.drawVector(g, particle.p, particle.f, pSys.bounds)
  }
}

case object Force1Renderer extends Renderer[Force1] {
  final val dragForceColor = new Color(0,0,255,10)

  def render(g: Graphics2D, pSys: ParticleSystem, force: Force1): Unit = {
    val p1 = pSys.getParticle(force.pId)

    force.forceFunc match {
      case _: Drag => g.setColor(dragForceColor)
      case _ => g.setColor(getForceColor(force))
    }

    RendererUtils.drawForce1(g, force, p1.p, pSys.bounds)
  }
}

case object Force2Renderer extends Renderer[Force2] {
  def render(g: Graphics2D, pSys: ParticleSystem, force: Force2): Unit = {
    val p1 = pSys.getParticle(force.p1Id)
    val p2 = pSys.getParticle(force.p2Id)

    RendererUtils.drawForce2(g, force, p1.p, p2.p, pSys.bounds)

    //    RendererUtils.drawVector3DAsText(
    //      g = g,
    //      pos = p2,
    //      value = gravity.force,
    //      bounds = bounds
    //    )
  }
}