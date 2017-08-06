package org.codingteam.icfpc2017

import ch.qos.logback.classic.{Level, LoggerContext}
import ch.qos.logback.classic.encoder.PatternLayoutEncoder
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.FileAppender
import org.slf4j.LoggerFactory
import org.slf4j.{Logger => SLF4Logger}

trait Logger[T] {

  val underlying: T

  val logger: AbstractLogger

  trait AbstractLogger {

    def info(formatOrMessage: String, args: Any*): Unit

    def debug(formatOrMessage: String, args: Any*): Unit

    def warn(formatOrMessage: String, args: Any*): Unit

    def trace(formatOrMessage: String, args: Any*): Unit

    def trace(formatOrMessage: String, ex: Throwable): Unit

  }

}

trait LogbackLogger extends Logger[SLF4Logger] {

  def loggerName: Option[String] = None

  def fromFactory = LoggerFactory.getLogger(this.getClass.getName)

  override val underlying = loggerName match {
    case Some(ln) => Appender(ln)
    case _        => fromFactory
  }


  object logger extends AbstractLogger {

    override def info(formatOrMessage: String, args: Any*) = {
      if(loggerName.isDefined)
        fromFactory.info(formatOrMessage, args)

      underlying.info(formatOrMessage, args)
    }

    override def debug(formatOrMessage: String, args: Any*) = {
      if(loggerName.isDefined)
        fromFactory.debug(formatOrMessage, args)

      underlying.debug(formatOrMessage, args)
    }

    override def warn(formatOrMessage: String, args: Any*) = {
      if(loggerName.isDefined)
        fromFactory.warn(formatOrMessage, args)

      underlying.warn(formatOrMessage, args)
    }

    override def trace(formatOrMessage: String, args: Any*) = {
      if(loggerName.isDefined)
        fromFactory.trace(formatOrMessage, args)

      underlying.trace(formatOrMessage, args)
    }

    override def trace(formatOrMessage: String, ex: Throwable) = {
      if (loggerName.isDefined)
        fromFactory.trace(formatOrMessage, ex)

      underlying.trace(formatOrMessage, ex)
    }

  }

  object Appender {

    def apply(name: String): SLF4Logger = {
      val context: LoggerContext = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
      val localLogger = context.getLogger(name)

      localLogger.setAdditive(false)

      val encoder = new PatternLayoutEncoder()
      encoder.setContext(context)
      encoder.setPattern("%-4relative [%thread] %-5level %logger{35} - %msg%n")
      encoder.start()

      val fileAppender = new FileAppender[ILoggingEvent]

      fileAppender.setAppend(false)
      fileAppender.setFile(s"logs/$name.log")
      fileAppender.setContext(context)
      fileAppender.setEncoder(encoder)
      fileAppender.start()

      localLogger.addAppender(fileAppender)
      localLogger.setLevel(Level.TRACE)
      localLogger.setAdditive(false)

      localLogger
    }

  }


}