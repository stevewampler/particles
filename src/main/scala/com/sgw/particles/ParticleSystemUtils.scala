package com.sgw.particles

/**
 * author: steve
 */
object ParticleSystemUtils {
  def bounds(particles: Seq[Particle]): Bounds3D = {
    val minMax: Pair[Vector3D, Vector3D] = particles.map(_.p).foldLeft(Pair(Vector3D.MaxValue, Vector3D.MinValue))((result: Pair[Vector3D, Vector3D], p: Vector3D) => {
      Pair(result._1.min(p), result._2.max(p))
    })
    Bounds3D(minMax._1, minMax._2)
  }
}
