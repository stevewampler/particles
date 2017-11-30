package com.sgw.particles.swing

import java.awt.geom.{Ellipse2D, Line2D}
import java.awt.{Color, Graphics2D}

import com.sgw.particles.model._

trait Renderer {
  def render(g: Graphics2D, bounds: Bounds3D): Unit
}

object Renderer {
  def apply(obj: Object): Renderer = obj match {
    case particle: Particle => ParticleRenderer(particle)
    case gravity: Gravity => GravityRenderer(gravity)
    case gravitationalForce: GravitationalForce => GravitationalForceRenderer(gravitationalForce)
    case spring: Spring => SpringRenderer(spring)
    case damper: Damper => DamperRenderer(damper)
    case springDamper: SpringDamper => SpringDamperRenderer(springDamper)
    case drag: Drag => DragRenderer(drag)
    case constant: ConstantForce => ConstantForceRenderer(constant)
    case _ => throw new IllegalArgumentException("unknown object type: " + obj)
  }
}

object RendererUtils {
  val BLACK = new Color(0, 0, 0)

  def calcScale(width: Int, height: Int, bounds: Bounds3D) = width.min(height) / bounds.width

  def getForceColor(force: Force): Color = {
    val forceMag = force.force.len
    val stress = (forceMag / force.maxForce).min(1.0)
    // stress 0-50 green to yellow (G+R), 50-100 yellow to red
    val red = (2 * stress * 255).toInt.min(255)
    val grn = (2 * (1.0 - stress) * 255).toInt.min(255)
    val blu = 0
    new Color(red, grn, blu)
  }

  def drawForce(g: Graphics2D, force: Force, p1: Vector3D, p2: Vector3D, bounds: Bounds3D): Unit = {
    if (force.broken) return
    val width = g.getClipBounds.width
    val height = g.getClipBounds.height
    val scale = calcScale(width, height, bounds)
    g.setColor(getForceColor(force))
    g.draw(
      new Line2D.Double(
        p1.x * scale + width / 2.0,
        -p1.y * scale + height / 2.0,
        p2.x * scale + width / 2.0,
        -p2.y * scale + height / 2.0
      )
    )
  }

  def drawVector3DAsText(g: Graphics2D, pos: Vector3D, value: Vector3D, bounds: Bounds3D): Unit = {
    g.setColor(BLACK)
    val width = g.getClipBounds.width
    val height = g.getClipBounds.height
    val scale = calcScale(width, height, bounds)
    val roundedValue = value.roundTo(2)
    g.drawString(
      value.toString(2),
      (pos.x * scale + width / 2.0 + 2).toInt,
      (-pos.y * scale + height / 2.0).toInt
    )
  }
}

object NullRenderer extends Renderer {
  def render(g: Graphics2D, bounds: Bounds3D) = {
  }
}

case class ParticleRenderer(particle: Particle) extends Renderer {
  def render(g: Graphics2D, bounds: Bounds3D) = {
    val width = g.getClipBounds.width
    val height = g.getClipBounds.height
    val scale = RendererUtils.calcScale(width, height, bounds)
    val size = (particle.radius * 2 * scale).toInt.min(20).max(2)
    g.setColor(Color.black)
    g.fill(new Ellipse2D.Double(particle.p.x * scale + width / 2.0 - size / 2.0, -particle.p.y * scale + height / 2.0 - size / 2.0, size, size))
  }
}

case class GravityRenderer(gravity: Gravity) extends Renderer {
  def render(g: Graphics2D, bounds: Bounds3D) = {
    val p1 = gravity.p.p
    val p2 = gravity.p.p + gravity.force

    RendererUtils.drawForce(g, gravity, p1, p2, bounds)

//    RendererUtils.drawVector3DAsText(
//      g = g,
//      pos = p2,
//      value = gravity.force,
//      bounds = bounds
//    )
  }
}

case class SpringRenderer(spring: Spring) extends Renderer {
  def render(g: Graphics2D, bounds: Bounds3D) = RendererUtils.drawForce(g, spring, spring.p1.p, spring.p2.p, bounds)
}

case class GravitationalForceRenderer(gravitationalForce: GravitationalForce) extends Renderer {
  def render(g: Graphics2D, bounds: Bounds3D) = RendererUtils.drawForce(g, gravitationalForce, gravitationalForce.p1.p, gravitationalForce.p2.p, bounds)
}

case class DamperRenderer(damper: Damper) extends Renderer {
  def render(g: Graphics2D, bounds: Bounds3D) = { /* RendererUtils.drawForce(g, damper, damper.p1.p, damper.p2.p, RendererUtils.getForceColor(damper), bounds) */ }
}

case class SpringDamperRenderer(springDamper: SpringDamper) extends Renderer {
  def render(g: Graphics2D, bounds: Bounds3D) = RendererUtils.drawForce(g, springDamper, springDamper.p1.p, springDamper.p2.p, bounds)
}

case class DragRenderer(drag: Drag) extends Renderer {
  def render(g: Graphics2D, bounds: Bounds3D) = {
    val p1 = drag.p.p
    val p2 = drag.p.p + drag.force

    RendererUtils.drawForce(g, drag, p1, p2, bounds)

//    RendererUtils.drawVector3DAsText(
//      g = g,
//      pos = p2,
//      value = drag.force,
//      bounds = bounds
//    )
  }
}

case class ConstantForceRenderer(constant: ConstantForce) extends Renderer {
  def render(g: Graphics2D, bounds: Bounds3D) = RendererUtils.drawForce(g, constant, constant.p.p, constant.p.p - constant.force, bounds)
}