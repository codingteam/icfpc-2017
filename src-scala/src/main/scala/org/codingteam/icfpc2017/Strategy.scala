package org.codingteam.icfpc2017

import java.io.{InputStream, OutputStream}

import org.codingteam.icfpc2017.Common.Punter
import org.codingteam.icfpc2017.Messages.Move


/**
  * Strategy of move selection.
  */
trait Strategy {

  private var _commonState: CommonState = new CommonState

  def commonState: CommonState = _commonState

  def commonState_=(s: CommonState): Unit = _commonState = s

  def map: GameMap.Map = commonState.map

  def me: Punter = commonState.me

  def graph: GraphMap = commonState.graph

  def nextMove(): Move

  def updateState(moves: Seq[Move])

  /**
    * Вероятность того, что данная стратегия сможет сделать хороший ход.
    * Эти вероятности учитываются нечётко, т.е. если стратегии имеют близкую
    * вероятность, то может быть выбрана любая.
    *
    * @return вероятность [0..1].
    */
  def goodMoveProbability(): Double

  /**
    * Read state from stream.
    *
    * @param is stream.
    */
  def read(is: InputStream): Unit = {}

  /**
    * Serialize state to stream.
    *
    * @param os stream.
    */
  def write(os: OutputStream): Unit = {}
}
