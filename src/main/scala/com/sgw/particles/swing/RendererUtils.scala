package com.sgw.particles.swing

import java.awt.{Color, Graphics2D}
import java.awt.geom.Line2D

import com.sgw.particles.model.{Bounds3D, Force, Vector3D}

object RendererUtils {
  val BLACK = new Color(0, 0, 0)

  def calcScale(width: Int, height: Int, bounds: Bounds3D) = width.min(height) / bounds.width

  def getForceColor(force: Force): Color = {
    val forceMag = force.value.len
    val stress = (forceMag / force.maxForce).min(1.0)
    // stress 0-50 green to yellow (G+R), 50-100 yellow to red
    val red = (2 * stress * 255).toInt.min(255)
    val grn = (2 * (1.0 - stress) * 255).toInt.min(255)
    val blu = 0
    new Color(red, grn, blu)
  }

  def drawVector(g: Graphics2D, p1: Vector3D, vector: Vector3D, bounds: Bounds3D): Unit = {
    val width = g.getClipBounds.width
    val height = g.getClipBounds.height
    val scale = calcScale(width, height, bounds)
    val p2 = vector + p1
    g.draw(
      new Line2D.Double(
        p1.x * scale + width / 2.0,
        -p1.y * scale + height / 2.0,
        p2.x * scale + width / 2.0,
        -p2.y * scale + height / 2.0
      )
    )
  }

  def drawForce1(g: Graphics2D, force: Force, p1: Vector3D, bounds: Bounds3D): Unit = {
    g.setColor(getForceColor(force))
    drawVector(g, p1, force.value, bounds)
  }

  def drawForce2(g: Graphics2D, force: Force, p1: Vector3D, p2: Vector3D, bounds: Bounds3D): Unit = {
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
