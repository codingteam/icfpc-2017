package org.codingteam

import java.io.{InputStream, OutputStream}
import java.net.Socket
import java.util.concurrent.CancellationException

import org.json4s.JsonAST.JValue

package object icfpc2017 {

  /**
    * Интерфейс классов, реализующих ввод/вывод (с сервером или в pipe).
    */
  trait StreamInterface extends AutoCloseable {

    def readFromServer(): JValue

    def writeToServer(data: JValue)

    def close(): Unit
  }

  /** Wrapper for in/out streams */
  trait SocketLike extends AutoCloseable {

    def inputStream: InputStream

    def outputStream: OutputStream

    def close(): Unit
  }

  object SocketLike {

    def fromSocket(socket: Socket): SocketLike = new TcpSocketLike(socket)

    def fromStdInOut(): SocketLike = new StdInOutSocketLike

    class TcpSocketLike(socket: Socket) extends SocketLike {

      override def inputStream: InputStream = socket.getInputStream

      override def outputStream: OutputStream = socket.getOutputStream

      override def close(): Unit = socket.close()
    }

    class StdInOutSocketLike extends SocketLike {

      override def inputStream: InputStream = System.in

      override def outputStream: OutputStream = System.out

      override def close(): Unit = {}
    }

  }

  object Parsing {

    object I {
      def unapply(arg: String): Option[Int] = {
        try {
          Some(arg.toInt)
        } catch {
          case _: NumberFormatException => None
        }
      }
    }

  }

  object Config {
    val MyPunterName = "codingpunter"
  }

  implicit class BooleanExt(val b: Boolean) extends AnyVal {
    def toOption[T](v: => T): Option[T] = if (b) Some(v) else None
  }

  class Canceller(val deadlineMs: Long) {
    @volatile var _isCancelled: Boolean = false

    def isCancelled = _isCancelled || {
      if (System.currentTimeMillis() >= deadlineMs)
        _isCancelled = true
      _isCancelled
    }

    def isCancelled_=(v: Boolean): Unit = _isCancelled = v

    def checkCancelled(): Unit = {
      if (isCancelled)
        throw new CancellationException("Cancelled")
    }

  }

}
