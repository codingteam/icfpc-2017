package org.codingteam.icfpc2017

import org.codingteam.icfpc2017.GameMap._
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization

import scala.io.Source

object Messages {

  implicit val formats = Serialization.formats(NoTypeHints)

  case class Punter(name: BigInt)

  abstract class Message {
  }

  trait Serializable {
    def toJson(): JObject
  }

  case class HelloRq(punter: String) extends Message with Serializable {
    def toJson(): JObject = {
      return ("me" -> punter)
    }
  }

  case class HelloRs(punter: String) extends Message

  object HelloRs {
    def unapply(json: JValue): Option[HelloRs] = {
      for {
        JString(name) <- (json \ "you").toOption
      } yield HelloRs(name)
    }
  }

  case class SetupRq(punter: BigInt, punters: Int, map: Map) extends Message

  object SetupRq {
    def unapply(json: JValue): Option[SetupRq] = {
      if (hasKey(json, "punter")) {
        return Some(json.extract[SetupRq])
      } else {
        return None
      }
    }
  }

  case class SetupRs(punter: Punter) extends Message with Serializable {
    def toJson(): JObject = {
      return ("ready" -> punter.name)
    }
  }

  abstract class Move extends Message with Serializable

  object Move {
    def unapply(json: JValue): Option[Move] = {
      return parseMove(json)
    }
  }

  case class MoveRq(moves: List[Move]) extends Message

  object MoveRq {
    def getMovesList(json: JValue): List[Move] = {
      (json \ "move" \ "moves").toOption match {
        case None => List()
        case Some(JArray(moves)) =>
          //println(moves)
          for {move <- moves} yield parseMove(move).get
        case default => List()
      }
    }

    def unapply(json: JValue): Option[MoveRq] = {
      if (hasKey(json, "move")) {
        val moves = getMovesList(json)
        return Some(MoveRq(moves))
      } else {
        return None
      }
    }
  }

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
    def unapply(json: JValue): Option[Claim] = {
      for {
        claim <- (json \ "claim").toOption
        JInt(name) <- (claim \ "punter").toOption
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
    def unapply(json: JValue): Option[Pass] = {
      for {
        JInt(name) <- (json \ "pass" \ "punter").toOption
      } yield Pass(Punter(name))
    }
  }

  case class Score(punter : Punter, score: Int) extends Message

  object Score {
    def unapply(json : JValue) : Option[Score] = {
      if (hasKey(json, "score")) {
        return Some(json.extract[Score])
      } else {
        return None
      }
    }
  }

  case class Stop(moves : List[Move], scores : List[Score]) extends Message

  object Stop {
    def unapply(json : JValue) : Option[Stop] = {
      if (hasKey(json, "stop")) {
        // FIXME
        return Some(Stop(List(), List()))
      } else {
        return None
      }
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
      case Pass(pass: Pass) => Some(pass)
      case Claim(claim: Claim) => Some(claim)
      case default => None
    }
  }

  def parseMoveStr(str: String): Option[Move] = {
    return parseMove(parse(str))
  }

  def parseServerMessageJson(json: JValue): Option[Message] = {
    json match {
      case HelloRs(hello: HelloRs) => Some(hello)
      case SetupRq(setup: SetupRq) => Some(setup)
      case MoveRq(move: MoveRq) => Some(move)
      case Stop(stop : Stop) => Some(stop)
      case default => None
    }
  }

  def parseServerMessageStr(str: String): Option[Message] = {
    return parseServerMessageJson(parse(str))
  }

  def parseServerMessageFile(path: String): Option[Message] = {
    val source = Source.fromFile(path)
    return parseServerMessageJson(parse(source.reader()))
  }
}
