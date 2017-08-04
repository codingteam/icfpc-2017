package org.codingteam.icfpc2017

object AppEntry extends App {

  private def run(): Unit = {
    args match {
      case Array("--test-map", mapFilePath) =>
        val map = GameMap.Map.fromJsonFile(mapFilePath)
        print(map.toGraph())
      case _ =>
        println("Hello!")
    }

  }

  run()
}
