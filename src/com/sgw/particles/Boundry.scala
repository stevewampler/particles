package com.sgw.particles

/**
 * author: steve
 */
trait Boundry {
  def apply(particle: Particle): Unit
  def apply(particles: Seq[Particle]): Unit = particles.foreach(apply(_))
}

case class Wall(origin: Vector3D, normal: Vector3D) extends Boundry {
  val norm = normal.normalize // just to make sure
  def apply(particle: Particle) = {
    val proj = (particle.p - origin).projectOnToNormal(norm)
    println("p=" + particle.p + " o=" + origin + " n=" + norm + " proj=" + proj)
  }
}
