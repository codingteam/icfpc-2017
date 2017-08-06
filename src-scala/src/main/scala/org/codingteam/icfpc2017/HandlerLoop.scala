package org.codingteam.icfpc2017

import java.io.EOFException

import org.codingteam.icfpc2017.Common.Punter
import org.codingteam.icfpc2017.Messages._
import org.codingteam.icfpc2017.strategy.Strategy
import org.codingteam.icfpc2017.futures.RandomFutureGenerator

/**
  * Message processing cycle.
  */
object HandlerLoop extends Logging {

  val futureGeneratorDistance = 3

  def runLoop(server: StreamInterface, strategy: Strategy, name: String): Unit = {
    try {
      val hello = HelloRq(name)
      val helloJson = hello.toJson()
      server.writeToServer(helloJson)
      // ignoring the response - nothing interesting there
      val helloResponse = server.readFromServer()
      val setupRequest = server.readFromServer()

      val fullState = Messages.parseServerMessageJson(setupRequest) match {
        case Some(setup: SetupRq) =>
          val punter = Punter(setup.punter)
          val futureGenerator = RandomFutureGenerator(setup.map, futureGeneratorDistance)
          val futures = setup.settings match {
            case Some(Settings(true)) => Some(futureGenerator.generate())
            case _ => None
          }
          log.debug(s"Generated futures: $futures.")
          val rs = SetupRs(punter, futures)
          val state = new FullState(CommonState(setup.map, setup.punter, setup.punters, setup.settings, futures), strategy)
          server.writeToServer(rs.toJson())
          state
        case _ =>
          new FullState(new CommonState, strategy)
      }
      strategy.commonState = fullState.commonState

      while (true) {
        val request = server.readFromServer()

        val response = Messages.parseServerMessageJson(request) match {
          case Some(move: MoveRq) =>
            fullState.updateState(move.moves)

            val m = strategy.nextMove()
            fullState.updateState(Seq(m))
            val fullfilledFutures = fullState.commonState.getFutureStats
            log.info(s"Our futures fullfilled: ${fullfilledFutures._1} of ${fullfilledFutures._2}")
            m

          case Some(stop: Stop) =>
            log.info("Our score: " + stop.getScore(fullState.commonState.me))
            //log.info(s"Resulting graph: ${fullState.commonState.graph}.")
            return

          case _ => Pass(fullState.commonState.me)
        }
        server.writeToServer(response.toJson())
      }

    } catch {
      case e: EOFException =>
        log.error("Exit during EOF from server", e)
      case t: Throwable =>
        log.error(s"Unknown error: $t", t)
    } finally {
      server.close()
    }
  }

  def runOfflineMove(server: StreamInterface, strategy: Strategy, name: String): Unit = {
    try {
      val hello = HelloRq(name)
      val helloJson = hello.toJson()
      server.writeToServer(helloJson)
      // ignoring the response - nothing interesting there
      val helloResponse = server.readFromServer()

      val request = server.readFromServer()
      Messages.parseServerMessageJson(request) match {
        case Some(setup: SetupRq) =>
          // -- realy fucking imperative code here --
          val futureGenerator = RandomFutureGenerator(setup.map, futureGeneratorDistance)
          val futures = setup.settings match {
            case Some(Settings(true)) => Some(futureGenerator.generate())
            case _ => None
          }
          val fullState = new FullState(CommonState(setup.map, setup.punter, setup.punters, setup.settings, futures), strategy)
          val rs = SetupRs(fullState.commonState.me, futures, fullState.toJson())
          strategy.commonState = fullState.commonState

          server.writeToServer(rs.toJson())

        case Some(move: MoveRq) =>
          // ...and here
          val fullState = new FullState(new CommonState, strategy)
          fullState.readFromJson(move.state)

          strategy.commonState = fullState.commonState

          fullState.updateState(move.moves)

          val nextMove = strategy.nextMove()

          fullState.updateState(Seq(nextMove))

          nextMove.state = fullState.toJson()
          server.writeToServer(nextMove.toJson())

        case Some(stop: Stop) =>
          val fullState = new FullState(new CommonState, strategy)
          fullState.readFromJson(stop.state)

          // no need to initialize strategy after stop.
          // strategy.commonState = fullState.commonState

          val me = fullState.commonState.me
          log.info(s"Our score: ${stop.getScore(me)}")
        case _ =>
      }

    } catch {
      case e: EOFException =>
        log.error("Exit during EOF from server", e)
      case t: Throwable =>
        log.error(s"Unknown error: $t", t)
    } finally {
      server.close()
    }
  }

}
