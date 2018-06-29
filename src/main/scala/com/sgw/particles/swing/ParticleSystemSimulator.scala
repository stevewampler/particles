package com.sgw.particles.swing

import com.sgw.particles.utils.Loggable
import play.api.libs.json.{JsError, JsSuccess, Json}
import twitter.scalding.Args

import scala.io.Source
import scala.swing.{MainFrame, SimpleSwingApplication}

object ParticleSystemSimulator extends SimpleSwingApplication with Loggable {
  private var maybeParticleSystemView: Option[ParticleSystemView] = None

  def top = maybeParticleSystemView.map { particleSystemView =>
    new MainFrame() {
      title = particleSystemView.particleSystemName
      contents = particleSystemView
    }
  }.getOrElse {
    throw new RuntimeException(
      "No particle system view."
    )
  }

  override def startup(args: Array[String]) {
    args.foreach(println)
    val psargs = Args(args)

    maybeParticleSystemView = psargs.getURI("file").map { uri =>
      if (uri.isAbsolute)
        uri.toURL
      else
        getClass.getResource(uri.getPath)
    }.map(Source.fromURL).map { source =>
      source.getLines().mkString("")
    }.map { json =>
      Json.fromJson[ParticleSystemView](Json.parse(json)) match {
        case JsSuccess(particleSystemView, _) => particleSystemView
        case JsError(errors) =>
          val errorStrings = errors.map { case (path, validationErrors) =>
            List(
              s"  ${path.toString()}:",
              validationErrors.map(_.toString).mkString("    ", "\n    ", "")
            ).mkString("\n")
          }
          throw new RuntimeException(s"Failed to read a particle system view.\n$errorStrings")
      }
    }

    super.startup(args)
  }
}
