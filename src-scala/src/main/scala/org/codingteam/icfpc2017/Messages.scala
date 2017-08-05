package org.codingteam.icfpc2017

import org.codingteam.icfpc2017.GameMap._
import org.codingteam.icfpc2017.Common._
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization

import scala.io.Source

object Messages {

  implicit val formats = Serialization.formats(NoTypeHints)


  abstract class Message {
  }

  trait Serializable {
    def toJson(): JObject
  }

  case class HelloRq(punter: String) extends Message with Serializable {
    def toJson(): JObject = {
      "me" -> punter
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
      hasKey(json, "punter") toOption json.extract[SetupRq]
    }
  }

  case class SetupRs(punter: Punter) extends Message with Serializable {
    def toJson(): JObject = {
      "ready" -> punter.id
    }
  }

  abstract class Move extends Message with Serializable

  object Move {
    def unapply(json: JValue): Option[Move] = {
      parseMove(json)
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
        case _ => List()
      }
    }

    def unapply(json: JValue): Option[MoveRq] = {
      hasKey(json, "move") toOption {
        val moves = getMovesList(json)
        MoveRq(moves)
      }
    }
  }

  case class Claim(punter: Punter, source: Site, target: Site) extends Move {
    def toJson(): JObject = {
      "claim" ->
        ("punter" -> punter.id) ~
          ("source" -> source.id) ~
          ("target" -> target.id)
    }
  }

  object IsClaim {
    def unapply(json: JValue): Option[Claim] = {
      for {
        claim <- (json \ "claim").toOption
        JInt(id) <- (claim \ "punter").toOption
        JInt(source) <- (claim \ "source").toOption
        JInt(target) <- (claim \ "target").toOption
      } yield Claim(Punter(id), Site(source), Site(target))
    }
    def unapply(x: Any): Option[(Punter, Site, Site)] = Claim.unapply(x)
  }

  case class Pass(punter: Punter) extends Move {
    def toJson(): JObject = {
      "pass" -> ("punter" -> punter.id)
    }
  }

  object Pass {
    def unapply(json: JValue): Option[Pass] = {
      for {
        JInt(id) <- (json \ "pass" \ "punter").toOption
      } yield Pass(Punter(id))
    }
  }

  case class Score(punter: Punter, score: Int) extends Message

  object Score {
    def unapply(json: JValue): Option[Score] = {
      hasKey(json, "score") toOption json.extract[Score]
    }
  }

  case class Stop(moves: List[Move], scores: List[Score]) extends Message

  object Stop {
    def unapply(json: JValue): Option[Stop] = {
      hasKey(json, "stop") toOption {
        // FIXME
        Stop(List(), List())
      }
    }
  }

  def hasKey(json: JValue, key: String): Boolean = {
    val v = json \ key
    // TODO: JNull check ?
    v != JNothing
  }

  def parseMove(json: JValue): Option[Move] = {
    json match {
      case Pass(pass) => Some(pass)
      case IsClaim(claim) => Some(claim)
      case _ => None
    }
  }

  def parseMoveStr(str: String): Option[Move] = {
    parseMove(parse(str))
  }

  def parseServerMessageJson(json: JValue): Option[Message] = {
    json match {
      case HelloRs(hello) => Some(hello)
      case SetupRq(setup) => Some(setup)
      case MoveRq(move) => Some(move)
      case Stop(stop) => Some(stop)
      case _ => None
    }
  }

  def parseServerMessageStr(str: String): Option[Message] = {
    parseServerMessageJson(parse(str))
  }

  def parseServerMessageFile(path: String): Option[Message] = {
    val source = Source.fromFile(path)
    parseServerMessageJson(parse(source.reader()))
  }
}
