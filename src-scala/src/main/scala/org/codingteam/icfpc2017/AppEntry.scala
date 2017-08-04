package org.codingteam.icfpc2017

object AppEntry extends App {

  private def run(): Unit = {
    args match {
      case Array("--test-map", mapFilePath) =>
        val map = GameMap.Map.fromJsonFile(mapFilePath)
        println(map.toGraph())

        val moveStr = """{"claim":{"punter":0,"source":0,"target":1}}"""
        val move = Messages.parseMoveStr(moveStr)
        println(move)
      case Array("--tcp", host, Parsing.I(port)) =>
        runTcpLoop(host, port)
      case _ =>
        println("Hello!")
    }

  }

  def runTcpLoop(host: String, port: Int): Unit = {
    HandlerLoop.runLoop(TcpInterface.connect(host, port), strategy)
  }

  // TODO: implement real strategy.

  lazy val strategy = new DelegatingStrategy(Seq(new DummyStrategy()))

  run()
}
