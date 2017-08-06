package org.codingteam.icfpc2017

import java.io._
import java.net.Socket

import org.json4s.JsonAST.JValue
import org.json4s.jackson.JsonMethods


/**
  * Parser of low-level messages in i/o streams.
  */
class StreamParser private(streams: SocketLike, logFileName: Option[String]) extends StreamInterface with Logging {

  override def readFromServer(): JValue = {
    val is = streams.inputStream

    def readByte(): Char = {
      val b = is.read()
      if (b == -1)
        throw new EOFException("End of stream")
      b.toChar
    }

    def getSize(): Int = {
      @inline def readCharAsInt(): Int = {
        val c = readByte()
        if (c.isDigit) c - '0' else -1
      }

      var n = 0
      var cn = readCharAsInt()
      while (cn != -1) {
        n = n * 10 + cn
        cn = readCharAsInt()
      }
      n
    }

    val n = getSize()
    val array: Array[Byte] = new Array[Byte](n)
    var alreadyRead = 0
    while (alreadyRead < n) {
      alreadyRead += is.read(array, alreadyRead, n - alreadyRead)
    }

    val input = {
      val reader = new InputStreamReader(new ByteArrayInputStream(array), "UTF-8")
      val sb = new java.lang.StringBuilder
      var c = reader.read()
      while (c != -1) {
        sb.append(c.toChar)
        c = reader.read()
      }
      sb.toString
    }
    log.debug("<-  " + input)
    logFileName match {
      case Some(fileName) =>
        new PrintWriter(new FileOutputStream(new File(fileName), true)) {
          write(input)
          close()
        }
      case None => ()
    }

    import org.json4s._
    JsonMethods.parse(input)
  }

  override def writeToServer(data: JValue): Unit = {
    val os = streams.outputStream
    val writer = new BufferedWriter(new OutputStreamWriter(os, "UTF-8"), 1024 * 1024)
    val str = JsonMethods.pretty(data)
    writer
      .append(String.valueOf(str.length))
      .append(':')
      .append(str)
    log.debug("->  " + str)
    writer.flush()
  }

  override def close(): Unit = streams.close()
}

object StreamParser {
  def connect(server: String, port: Int, log: Option[String]): StreamParser = {
    val socket = new Socket(server, port)
    new StreamParser(SocketLike.fromSocket(socket), log)
  }

  def connectToStdInOut(log: Option[String]): StreamParser = {
    new StreamParser(SocketLike.fromStdInOut(), log)
  }

}
