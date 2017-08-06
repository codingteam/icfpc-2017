package org.codingteam.icfpc2017.onlinegamer

import java.time.Instant

import akka.actor.{Actor, Props}
import org.codingteam.icfpc2017.strategy.Strategy
import org.codingteam.icfpc2017.{HandlerLoop, StreamParser}

object RunLoopActor {

  def props(host: String, port: Int, name: String, strategy: Strategy) =
    Props(new RunLoopActor(host, port, name, strategy))

}

class RunLoopActor(host: String, port: Int, name: String, strategy: Strategy) extends Actor {

  override def receive: Receive = {
    case "loop" =>
      try {
        HandlerLoop.runLoop(StreamParser.connect(host, port, Some(s"logs/online-gamer-${Instant.now().toEpochMilli}.lson")), strategy, name)
      } catch {
        case ex: Exception =>
          ex.printStackTrace()
      } finally {
        sender() ! MainActor.LoopStop(port)
      }


  }
}