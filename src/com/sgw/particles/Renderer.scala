package com.sgw.particles

import java.awt.{Shape, Color, Graphics2D}
import java.awt.geom.{Line2D, Ellipse2D}

abstract class Renderer extends CanDie {
  def render(g: Graphics2D, bounds: Bounds3D, width: Int, height: Int, scale: Double): Unit
  def dead: Boolean
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
    case rocket: ParticleFunctionForce => ParticleFunctionForceRenderer(rocket)
    case _ => throw new IllegalArgumentException("unknown object type: " + obj)
  }
}

object RendererUtils {
  def calcScale(width: Int, height: Int, bounds: Bounds3D) = width.min(height) / bounds.width

  def getValueColor(value: Double, maxValue: Double) = {
    val stress = (value / maxValue).min(1.0)
    // stress 0-50 green to yellow (G+R), 50-100 yellow to red
    val red = (2 * stress * 255).toInt.min(255)
    val grn = (2 * (1.0 - stress) * 255).toInt.min(255)
    val blu = 0
    new Color(red, grn, blu)
  }

  def getForceMagnitudeColor(force: Force) = getValueColor(force.force.len, force.maxForce)

  // TODO: create a Age trait
  def getForceAgeColor(force: Force) = getValueColor(force.age, force.maxAge)
  def getParticleAgeColor(particle: Particle) = getValueColor(particle.age, particle.maxAge)

  def drawForce(g: Graphics2D, force: Force, p1: Vector3D, p2: Vector3D, bounds: Bounds3D, width: Int, height: Int, scale: Double) {
    g.setColor(getForceMagnitudeColor(force))
    g.draw(new Line2D.Double(p1.x * scale + width / 2.0, -p1.y * scale + height / 2.0, p2.x * scale + width / 2.0, -p2.y * scale + height / 2.0))
  }


  def defaultParticleColorFunc(particle: Particle) = Color.black

  def defaultParticleShapeFunc(particle: Particle, width: Int, height: Int, scale: Double) = {
    val size = (particle.radius * 2 * scale).toInt.min(20).max(2)
    new Ellipse2D.Double(particle.p.x * scale + width / 2.0 - size / 2.0, -particle.p.y * scale + height / 2.0 - size / 2.0, size, size)
  }
}

object NullRenderer extends Renderer {
  def render(g: Graphics2D, bounds: Bounds3D, width: Int, height: Int, scale: Double) = {}
  def dead = true
}

case class ParticleRenderer(
    particle: Particle,
    colorFunc: (Particle) => Color = RendererUtils.defaultParticleColorFunc,
    shapeFunc: (Particle, Int, Int, Double) => Shape = RendererUtils.defaultParticleShapeFunc
) extends Renderer {
  def render(g: Graphics2D, bounds: Bounds3D, width: Int, height: Int, scale: Double) = {
    g.setColor(colorFunc(particle))
    g.fill(shapeFunc(particle, width, height, scale))
  }

  def dead = particle.dead
}

abstract class ForceRenderer(val force: Force) extends Renderer {
  def dead = force.dead
}

case class GravityRenderer(gravity: Gravity) extends ForceRenderer(gravity) {
  def render(g: Graphics2D, bounds: Bounds3D, width: Int, height: Int, scale: Double) =
    RendererUtils.drawForce(g, gravity, gravity.p.p, gravity.p.p - gravity.force, bounds, width, height, scale)
}

case class SpringRenderer(spring: Spring) extends ForceRenderer(spring) {
  def render(g: Graphics2D, bounds: Bounds3D, width: Int, height: Int, scale: Double) =
    RendererUtils.drawForce(g, spring, spring.p1.p, spring.p2.p, bounds, width, height, scale)
}

case class GravitationalForceRenderer(gravitationalForce: GravitationalForce) extends ForceRenderer(gravitationalForce) {
  def render(g: Graphics2D, bounds: Bounds3D, width: Int, height: Int, scale: Double) =
    RendererUtils.drawForce(g, gravitationalForce, gravitationalForce.p1.p, gravitationalForce.p2.p, bounds, width, height, scale)
}

case class DamperRenderer(damper: Damper) extends ForceRenderer(damper) {
  def render(g: Graphics2D, bounds: Bounds3D, width: Int, height: Int, scale: Double) =
  { /* RendererUtils.drawForce(g, damper, damper.p1.p, damper.p2.p, RendererUtils.getForceMagnitudeColor(damper), bounds) */ }
}

case class SpringDamperRenderer(springDamper: SpringDamper) extends ForceRenderer(springDamper) {
  def render(g: Graphics2D, bounds: Bounds3D, width: Int, height: Int, scale: Double) =
    RendererUtils.drawForce(g, springDamper, springDamper.p1.p, springDamper.p2.p, bounds, width, height, scale)
}

case class DragRenderer(drag: Drag) extends ForceRenderer(drag) {
  def render(g: Graphics2D, bounds: Bounds3D, width: Int, height: Int, scale: Double) =
    RendererUtils.drawForce(g, drag, drag.p.p, drag.p.p - drag.force, bounds, width, height, scale)
}

case class ConstantForceRenderer(constant: ConstantForce) extends ForceRenderer(constant) {
  def render(g: Graphics2D, bounds: Bounds3D, width: Int, height: Int, scale: Double) =
    RendererUtils.drawForce(g, constant, constant.p.p, constant.p.p - constant.force, bounds, width, height, scale)
}

case class ParticleFunctionForceRenderer(particleFunctionForce: ParticleFunctionForce) extends ForceRenderer(particleFunctionForce) {
  def render(g: Graphics2D, bounds: Bounds3D, width: Int, height: Int, scale: Double) =
    RendererUtils.drawForce(
      g,
      particleFunctionForce,
      particleFunctionForce.p.p,
      particleFunctionForce.p.p - particleFunctionForce.force,
      bounds,
      width,
      height,
      scale
    )
}
