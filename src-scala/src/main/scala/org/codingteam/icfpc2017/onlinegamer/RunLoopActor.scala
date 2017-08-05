package org.codingteam.icfpc2017.onlinegamer

import java.time.Instant

import akka.actor.{Actor, Props}
import akka.actor.Actor.Receive
import org.codingteam.icfpc2017.{HandlerLoop, LogbackLogger, Strategy, TcpInterface}

object RunLoopActor {

  def props(host: String, port: Int, name: String, strategy: Strategy) =
    Props(new RunLoopActor(host, port, name, strategy))

}

class RunLoopActor(host: String, port: Int, name: String, strategy: Strategy) extends Actor with LogbackLogger {

  override def loggerName: Option[String] = Some(s"bot-$name-$port-${strategy.getClass.getCanonicalName}")

  override def receive: Receive = {
    case "loop" =>
      try {
        HandlerLoop(loggerName).runLoop(TcpInterface.connect(host, port, loggerName), strategy, name, offline = false)
      } catch {
        case ex: Exception =>
          logger.trace("Exception", ex)
      } finally {
        sender() ! MainActor.LoopStop(port)
      }


  }
}