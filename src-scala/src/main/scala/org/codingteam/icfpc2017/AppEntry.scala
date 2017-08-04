package org.codingteam.icfpc2017

import org.codingteam.icfpc2017._

object AppEntry extends App{

  private def run(mapFilePath : String): Unit ={
    // println("Hello, world!")
    val map = GameMap.Map.fromJsonFile(mapFilePath)
    print(map.toJson())
    print(map.sites)
  }

  override def main(args : Array[String]) : Unit = {
    run(args(0))
  }
}
