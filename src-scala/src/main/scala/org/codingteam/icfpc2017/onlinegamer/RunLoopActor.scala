package org.codingteam.icfpc2017.onlinegamer

import akka.actor.{Actor, Props}
import akka.actor.Actor.Receive
import org.codingteam.icfpc2017.{HandlerLoop, Strategy, TcpInterface}

object RunLoopActor {

  def props(host: String, port: Int, strategy: Strategy) =
    Props(new RunLoopActor(host, port, strategy))

}

class RunLoopActor(host: String, port: Int, strategy: Strategy) extends Actor {

  override def receive: Receive = {
    case "loop" =>
      try {
        HandlerLoop.runLoop(TcpInterface.connect(host, port, None), strategy, offline = false)
      } catch {
        case ex: Exception =>
          ex.printStackTrace()
      } finally {
        sender() ! MainActor.LoopStop(port)
      }


  }
}