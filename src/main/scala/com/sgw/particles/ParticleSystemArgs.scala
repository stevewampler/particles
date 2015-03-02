package com.sgw.particles

import twitter.scalding.Args
import scala.io.Source
import com.sgw.particles.utils.JSON

case class ParticleSystemArgs(args: Array[String]) {
  private val psargs = Args(args)

  lazy val config = psargs.getURI("config")
    .map(configURI => if (configURI.isAbsolute) configURI.toURL else getClass.getResource(configURI.getPath))
    .map(Source.fromURL)
    .map(_.getLines().mkString(""))
    .flatMap(JSON.parseFull)
    .getOrElse(
      throw new RuntimeException(
        "Failed to create or read a particle system config file. The --config parameter ('" +
        psargs.getURI("config").getOrElse("") +
        "') is likely invalid.")
    )
}
