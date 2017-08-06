package org.codingteam.icfpc2017

import java.io.{InputStream, OutputStream}

import org.codingteam.icfpc2017.Messages.Move

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
class MixedStrategy(val strategies: Seq[(Double, Strategy)]) extends Strategy {
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
  override def nextMove(): Move = {
    val ps = strategies.map(s => (s._2, s._2.goodMoveProbability() * s._1))
    val W = ps.map(_._2).sum

    var probability = rnd.nextDouble()
    for (s <- ps) {
      probability -= s._2 / W
      if (probability <= 0) {
        println(s"Mixed: Selected strategy: ${s._1}")
        return s._1.nextMove()
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
