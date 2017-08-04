package org.codingteam.icfpc2017

import org.codingteam.icfpc2017._

object AppEntry extends App{

  private def run(mapFilePath : String): Unit ={
    // println("Hello, world!")
    val map = GameMap.Map.fromJsonFile(mapFilePath)
    print(map.toJson())
  }

  run(args(0))
}
