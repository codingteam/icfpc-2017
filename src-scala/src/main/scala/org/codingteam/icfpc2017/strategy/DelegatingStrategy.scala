package org.codingteam.icfpc2017.strategy

import java.io.{InputStream, OutputStream}
import java.util.concurrent.TimeUnit

import org.codingteam.icfpc2017.Messages.Move
import org.codingteam.icfpc2017.{Canceller, CommonState, ExecutionContexts, Logging, Messages}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, CancellationException, Future, TimeoutException}
import scala.util.Random

/**
  * Основная стратегия, делегирующая выбор хода стратегии из списка.
  * Выбирается та стратегия, для которой текущая ситуация наиболее удобна.
  */
class DelegatingStrategy(val strategies: Seq[Strategy],
                         val useBackgroundThreads: Boolean) extends Strategy with Logging {
  require(strategies.nonEmpty)

  private val rnd = new Random()

  override def commonState_=(s: CommonState): Unit = {
    super.commonState_=(s)
    strategies foreach (_.commonState = s)
  }

  override def nextMove(deadLineMs: Long, cancel: Canceller): Move = {
    val ps = strategies.map(s => (s, s.goodMoveProbability())) sortBy (v => -v._2)
    // случайно выбираем из наилучших стратегий, если они имеют близкие вероятности.
    val best = ps.head
    val Accuracy = 0.05
    val good = ps.takeWhile(_._2 >= best._2 - Accuracy)
    assert(good.nonEmpty)
    val selected = good(rnd nextInt good.size)._1

    @inline def getMove(s: Strategy): Move = {
      log.debug(s"Delegating: Selected strategy: $s")
      if (useBackgroundThreads) {
        val fut = Future(s.nextMove(deadLineMs, cancel))(ExecutionContexts.backgroundContext)
        try {
          Await.result(fut, Duration(deadLineMs - System.currentTimeMillis(), TimeUnit.MILLISECONDS))
        } catch {
          case _: TimeoutException =>
            log.debug("Deadline timeout, return Pass()")
            Messages.Pass(me)
          case _: InterruptedException =>
            log.debug("Wait on nextMove() interrupted, return Pass()")
            Messages.Pass(me)
          case _: CancellationException =>
            log.debug("nextMove() cancelled, return Pass()")
            Messages.Pass(me)
        }
      } else s.nextMove(deadLineMs, cancel)
    }

    try {
      selected.nextMove(deadLineMs, cancel)
    } catch {
      case _: CancellationException => Messages.Pass(me)
    }
  }

  override def updateState(moves: Seq[Move]): Unit = {
    strategies foreach (_.updateState(moves))
  }

  override def goodMoveProbability(): Double = 0.95 // no ideas about actual value in this case.

  override def read(is: InputStream): Unit = {
    strategies foreach (_.read(is))
  }

  override def write(os: OutputStream): Unit = {
    strategies foreach (_.write(os))
  }
}
