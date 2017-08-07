package org.codingteam.icfpc2017

object Common {
  case class Punter (var id : BigInt) {
    var name : String = "";
  }
  case class Label(var punter: Punter, var option: Option[Punter])
}