package org.codingteam.icfpc2017.strategy

import java.io.{InputStream, OutputStream}
import java.util.concurrent.TimeUnit

import org.codingteam.icfpc2017.Messages.Move
import org.codingteam.icfpc2017.{Canceller, CommonState, ExecutionContexts, Logging, Messages}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, CancellationException, Future, TimeoutException}
import scala.util.Random

/**
  * Смешивает заданные стратегии с заданными весами.
  * Также учитывает good move probability, возвращаемую каждой стратегией:
  *
  * P(X) = (w_X \cdot p_X) / (\sum_{i \in S} w_i \cdot p_i)
  *
  * где P(X) - вероятность использования стратегии X
  * w_i  - вес стратегии i
  * p_i  - good move probability, которую вернула стратегия i
  * S    - множество всех стратегий
  */
class MixedStrategy(val strategies: Seq[(Double, Strategy)],
                    val useBackgroundThreads: Boolean) extends Strategy with Logging {
  require(strategies.nonEmpty)

  private val W = strategies.map(_._1).sum

  private val rnd = new Random()

  override def commonState_=(s: CommonState): Unit = {
    super.commonState_=(s)
    strategies foreach (_._2.commonState = s)
  }

  //  * P(X) = (w_X \cdot p_X) / (\sum_{i \in S} w_i \cdot p_i)
  //  *
  //  * где P(X) - вероятность использования стратегии X
  //  *     w_i  - вес стратегии i
  //  *     p_i  - good move probability, которую вернула стратегия i
  //  *     S    - множество всех стратегий
  override def nextMove(deadLineMs: Long, cancel: Canceller): Move = {
    val ps = strategies.map(s => (s._2, s._2.goodMoveProbability() * s._1))
    val W = ps.map(_._2).sum

    @inline def getMove(s: Strategy): Move = {
      log.debug(s"Mixed: Selected strategy: $s")
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

    var probability = rnd.nextDouble()
    for (s <- ps) {
      probability -= s._2 / W
      if (probability <= 0) {
        return getMove(s._1)
      }
    }
    Messages.Pass(me)
  }

  override def updateState(moves: Seq[Move]): Unit = {
    strategies foreach (_._2.updateState(moves))
  }

  override def goodMoveProbability(): Double = 1


  override def read(is: InputStream): Unit = {
    strategies foreach (_._2.read(is))
  }

  override def write(os: OutputStream): Unit = {
    strategies foreach (_._2.write(os))
  }
}
