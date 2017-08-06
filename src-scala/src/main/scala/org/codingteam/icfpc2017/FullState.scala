package org.codingteam.icfpc2017

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream}
import java.util.Base64

import org.codingteam.icfpc2017.strategy.Strategy
import org.json4s.JsonAST.{JString, JValue}

/**
  * Class for full game state save/restore.
  */
class FullState(val commonState: CommonState,
                val strategy: Strategy) extends Logging {
  // "CT"
  val magic: Array[Byte] = Array[Byte](67, 84)

  def read(is: InputStream): Unit = {
    val correctMagic = magic.forall(_ == is.read().toByte)
    if (!correctMagic) {
      log.error("Wrong magic value, can`t read state")
      throw new IllegalArgumentException("Wrong magic value")
    }
    commonState.read(is)
    strategy.read(is)
  }

  def write(os: OutputStream): Unit = {
    os.write(magic)
    commonState.write(os)
    strategy.write(os)
  }

  def toJson(): JValue = {
    val os = new ByteArrayOutputStream()
    write(os)
    JString(Base64.getEncoder.encodeToString(os.toByteArray))
  }

  def readFromJson(json: JValue): Unit = {
    json match {
      case JString(str) =>
        read(new ByteArrayInputStream(Base64.getDecoder.decode(str)))
      case _ =>
        log.error("cant read state from json")
        throw new IllegalArgumentException("Wrong state format")
    }
  }
}