package com.sgw.particles

case class ParticleSystem(
    var particles: Seq[Particle],
    var forces: Seq[Force],
    var boundaries: Seq[Boundry] = List(),
    bounds: Bounds3D) extends Updatable {
  var onUpdatedListener: (ParticleSystem, Double) => Unit = null

  override def beginUpdate(t: Double) = particles.foreach(_.beginUpdate(t))

  def update(t: Double) = {
    forces.foreach(_.apply(t))
    particles.foreach(_.update(t))
    boundaries.foreach(_.apply(particles))
  }

  override def endUpdate(t: Double) = {
    forces = forces.filter(!_.dead)
    particles = particles.filter(!_.dead)
    if (onUpdatedListener != null) onUpdatedListener(this, t)
  }
}

