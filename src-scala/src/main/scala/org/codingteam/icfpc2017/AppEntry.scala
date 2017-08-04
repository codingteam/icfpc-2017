package org.codingteam.icfpc2017

object AppEntry extends App {

  private def run(): Unit = {
    args match {
      case Array("--test-map", mapFilePath) =>
        val map = GameMap.Map.fromJsonFile(mapFilePath)
        print(map.toGraph())

        val moveStr = """{"claim":{"punter":0,"source":0,"target":1}}"""
        val move = Messages.parseMoveStr(moveStr)
        print(move)

      case _ =>
        println("Hello!")
    }

  }

  run()
}
