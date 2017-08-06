package org.codingteam.icfpc2017

import java.io.PrintStream

/**
  * Trait which initializes instance of logger with object's classname.
  */
trait Logging {

  lazy val log: Logging.Logger = Logging.createLogger(getClass.getName)

}

object Logging {

  def createLogger(name: String): Logger = outputStream map (new StreamLogger(name, _)) getOrElse NullLogger

  var outputStream: Option[PrintStream] = None

  trait Logger {
    def debug(msg: String): Unit

    def info(msg: String): Unit

    def error(msg: String, t: Throwable = null): Unit

    def debug(msg: Any): Unit = debug(msg.toString)
  }

  object NullLogger extends Logger {
    override def debug(msg: String): Unit = {}

    override def info(msg: String): Unit = {}

    override def error(msg: String, cause: Throwable = null): Unit = {}
  }

  class StreamLogger(name: String, out: PrintStream) extends Logger {

    /** Name in converted form (e.g. java.util.List => j.u.List).  */
    lazy val convertedName: String = {
      val components = name split "\\."
      components.take(components.size - 1).map(_ take 1) :+ components.last mkString "."
    }

    override def debug(msg: String): Unit = {
      out.println(s"DEBUG $convertedName: $msg")
    }

    override def info(msg: String): Unit = {
      out.println(s"INFO $convertedName: $msg")
    }

    override def error(msg: String, cause: Throwable = null): Unit = {
      out.println(s"ERROR $convertedName: $msg")
      if (cause ne null)
        cause.printStackTrace(out)
    }

  }

}