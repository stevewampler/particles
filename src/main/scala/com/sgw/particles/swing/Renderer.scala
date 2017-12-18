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
//    case gravity: Gravity => GravityRenderer(gravity)
//    case gravitationalForce: GravitationalForce => GravitationalForceRenderer(gravitationalForce)
//    case spring: Spring => SpringRenderer(spring)
//    case damper: Damper => DamperRenderer(damper)
//    case springDamper: SpringDamper => SpringDamperRenderer(springDamper)
//    case drag: DragParticleFunction1 => DragRenderer(drag)
//    case constant: ConstantForceParticleFunction1 => ConstantForceRenderer(constant)
//    case _ => throw new IllegalArgumentException("unknown object type: " + obj)
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

//case class Force1Renderer(force: Force1) extends Renderer {
//  def render(g: Graphics2D, bounds: Bounds3D) = {
//    val p1 = force.pId.p.p
//    val p2 = gravity.p.p + gravity.force
//
//    RendererUtils.drawForce(g, gravity, p1, p2, bounds)
//
////    RendererUtils.drawVector3DAsText(
////      g = g,
////      pos = p2,
////      value = gravity.force,
////      bounds = bounds
////    )
//  }
//}
//
//case class SpringRenderer(spring: Spring) extends Renderer {
//  def render(g: Graphics2D, bounds: Bounds3D) = RendererUtils.drawForce(g, spring, spring.p1.p, spring.p2.p, bounds)
//}
//
//case class GravitationalForceRenderer(gravitationalForce: GravitationalForce) extends Renderer {
//  def render(g: Graphics2D, bounds: Bounds3D) = RendererUtils.drawForce(g, gravitationalForce, gravitationalForce.p1.p, gravitationalForce.p2.p, bounds)
//}
//
//case class DamperRenderer(damper: Damper) extends Renderer {
//  def render(g: Graphics2D, bounds: Bounds3D) = { /* RendererUtils.drawForce(g, damper, damper.p1.p, damper.p2.p, RendererUtils.getForceColor(damper), bounds) */ }
//}
//
//case class SpringDamperRenderer(springDamper: SpringDamper) extends Renderer {
//  def render(g: Graphics2D, bounds: Bounds3D) = RendererUtils.drawForce(g, springDamper, springDamper.p1.p, springDamper.p2.p, bounds)
//}
//
//case class DragRenderer(drag: DragParticleFunction1) extends Renderer {
//  def render(g: Graphics2D, bounds: Bounds3D) = {
//    val p1 = drag.p.p
//    val p2 = drag.p.p + drag.force
//
//    RendererUtils.drawForce(g, drag, p1, p2, bounds)
//
////    RendererUtils.drawVector3DAsText(
////      g = g,
////      pos = p2,
////      value = drag.force,
////      bounds = bounds
////    )
//  }
//}
//
//case class ConstantForceRenderer(constant: ConstantForceParticleFunction1) extends Renderer {
//  def render(g: Graphics2D, bounds: Bounds3D) = RendererUtils.drawForce(g, constant, constant.p.p, constant.p.p - constant.force, bounds)
//}
