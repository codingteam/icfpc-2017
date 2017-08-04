package org.codingteam.icfpc2017

import org.codingteam.icfpc2017.GameMap._
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization

object Messages {

  implicit val formats = Serialization.formats(NoTypeHints)

  case class Punter(name: String)

  object Punter {
    lazy val Me = Punter(Config.MyPunterName)
  }

  abstract class Message {
  }

  trait Serializable {
    def toJson(): JObject
  }

  case class HelloRq(punter: Punter) extends Message with Serializable {
    def toJson(): JObject = {
      return ("me" -> punter.name)
    }
  }

  case class HelloRs(punter: Punter) extends Message

  case class SetupRq(punterName: String, punters: Int, map: Map) extends Message

  case class SetupRs(punter: Punter) extends Message with Serializable {
    def toJson(): JObject = {
      return ("ready" -> punter.name)
    }
  }

  abstract class Move extends Message with Serializable

  case class MoveRq(moves: List[Move])

  case class Claim(punter: Punter, source: Site, target: Site) extends Move {
    def toJson(): JObject = {
      return ("claim" ->
        ("punter" -> punter.name) ~
          ("source" -> source.id) ~
          ("target" -> target.id)
        )
    }
  }

  object Claim {
    def unapply(json : JValue) : Option[Claim] = {
      for {
        claim <- (json \ "claim").toOption
        JString(name) <- (claim \ "punter").toOption
        JInt(source) <- (claim \ "source").toOption
        JInt(target) <- (claim \ "target").toOption
      } yield Claim(Punter(name), Site(source), Site(target))
    }

  }

  case class Pass(punter: Punter) extends Move {
    def toJson(): JObject = {
      return ("pass" -> ("punter" -> punter.name))
    }
  }

  object Pass {
    def unapply(json : JValue) : Option[Pass] = {
      for {
        JString(name) <- (json \ "pass").toOption
      } yield Pass(Punter(name))
    }

  }

  def hasKey(json: JValue, key: String): Boolean = {
    if ((json \ key) != JNothing) {
      return true
    } else {
      return false
    }
  }

  def parseMove(json: JValue): Option[Move] = {
    json match {
      case Pass(pass : Pass) => Some(pass)
      case Claim(claim : Claim) => Some(claim)
      case default => None
    }

  }

  def parseMoveStr(str: String): Option[Move] = {
    return parseMove(parse(str))
  }

  def parseServerMessageJson(json: JValue): Option[Message] = {
    for {
      JString(name) <- (json \ "you").toOption
    } yield HelloRs(Punter(name))

    if (hasKey(json, "punter")) {
      return Some(json.extract[SetupRq])
    }

    return None
  }
}
