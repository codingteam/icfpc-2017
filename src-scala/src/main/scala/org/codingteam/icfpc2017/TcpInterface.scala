package org.codingteam.icfpc2017

import java.io._
import java.net.Socket

import org.json4s.JsonAST.JValue
import org.json4s.jackson.JsonMethods


/**
  * TCP handler.
  */
class TcpInterface private(socket: Socket) extends StreamInterface {

  // TODO: copy this impl to generic handler.

  override def readFromServer(): JValue = {
    val is = socket.getInputStream

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
    var array: Array[Byte] = new Array[Byte](n)
    is.read(array, 0, n)
    val input = new String(array)
    import org.json4s._
    JsonMethods.parse(input)
  }

  override def writeToServer(data: JValue): Unit = {
    val os = socket.getOutputStream
    val writer = new BufferedWriter(new OutputStreamWriter(os, "ANSI"), 1024 * 1024)
    val str = JsonMethods.pretty(data)
    writer
      .append(String.valueOf(str.length))
      .append(':')
      .append(str)
    writer.flush()
  }

  override def close(): Unit = socket.close()
}

object TcpInterface {
  def connect(server: String, port: Int): TcpInterface = {
    val socket = new Socket(server, port)
    new TcpInterface(socket)
  }
}
