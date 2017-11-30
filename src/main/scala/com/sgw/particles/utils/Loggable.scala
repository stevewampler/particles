package com.sgw.particles.utils

// import org.slf4j.LoggerFactory

/**
 * A mix-in trait used to give a class the ability to log stuff.
 * To use it, do this
 * <pre>
 *   class MyClass extends SomethingElse with Loggable {
 *     ...
 *     debug("Log some debug information")
 *     info("Log some information")
 *     warn("Log a warning")
 *     error("Log an error")
 *     ...
 *   }
 * </pre>
 */
trait Loggable {
  self =>

  // doing this for now so I don't have to messing with a logging framework
  private case class Logger(cls: Class[_]) {
    val isDebugEnabled = true
    val isInfoEnabled  = true
    val isWarnEnabled  = true
    val isErrorEnabled = true

    private def logMsg(level: String, msg: String) = level + " (" + cls.getName + "): " + msg

    def debug(msg: String): Unit = println(logMsg("DEBUG", msg))
    def info (msg: String): Unit = println(logMsg("INFO ", msg))
    def warn (msg: String): Unit = println(logMsg("WARN ", msg))
    def error(msg: String): Unit = println(logMsg("ERROR", msg))

    def debug(msg: String, t: Throwable): Unit = { debug(msg); t.printStackTrace() }
    def info (msg: String, t: Throwable): Unit = { info (msg); t.printStackTrace() }
    def warn (msg: String, t: Throwable): Unit = { warn (msg); t.printStackTrace() }
    def error(msg: String, t: Throwable): Unit = { error(msg); t.printStackTrace() }
  }

  private val log = Logger(self.getClass) // LoggerFactory.getLogger(self.getClass)

  def debug[T](msg: => T): Unit = if (log.isDebugEnabled) log.debug(msg.toString)
  def info [T](msg: => T): Unit = if (log.isInfoEnabled)  log.info(msg.toString)
  def warn [T](msg: => T): Unit = if (log.isWarnEnabled)  log.warn(msg.toString)
  def error[T](msg: => T): Unit = if (log.isErrorEnabled) log.error(msg.toString)

  def debug[T](msg: => T, t: Throwable): Unit = if (log.isDebugEnabled) log.debug(msg.toString, t)
  def info [T](msg: => T, t: Throwable): Unit = if (log.isInfoEnabled)  log.info(msg.toString, t)
  def warn [T](msg: => T, t: Throwable): Unit = if (log.isWarnEnabled)  log.warn(msg.toString, t)
  def error[T](msg: => T, t: Throwable): Unit = if (log.isErrorEnabled) log.error(msg.toString, t)
}
