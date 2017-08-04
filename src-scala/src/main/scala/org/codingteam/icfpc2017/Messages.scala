package org.codingteam.icfpc2017

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization

import org.codingteam.icfpc2017.GameMap._

object Messages {

  case class Punter(name: String)

  abstract class Message {
  }

  trait Serializable {
    def toJson() : JObject
  }

  case class HelloRq(punter: Punter) extends Message with Serializable {
    def toJson(): JObject = {
      return ("me" -> punter.name)
    }
  }

  case class HelloRs(punter: Punter) extends Message with Serializable {
    def toJson(): JObject = {
      return ("you" -> punter.name)
    }
  }

  case class SetupRs(punter : Punter) extends Message with Serializable {
    def toJson() : JObject = {
      return ("ready" -> punter.name)
    }
  }

  case class Claim(punter : Punter, source : Site, target : Site) extends Message with Serializable {
    def toJson() : JObject = {
      return ("claim" ->
        ("punter" -> punter.name) ~
        ("source" -> source.id) ~
        ("target" -> target.id)
      )
    }
  }

  case class Pass(punter : Punter) extends Message with Serializable {
    def toJson() : JObject = {
      return ("pass" -> ("punter" -> punter.name))
    }
  }
}
