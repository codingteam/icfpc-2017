package org.codingteam.icfpc2017

import java.util.concurrent._

import scala.concurrent.{ExecutionContext, Future}

object ExecutionContexts {
  lazy implicit val backgroundContext: ExecutionContext =
    ExecutionContext.fromExecutorService(new ThreadPoolExecutor(
      Runtime.getRuntime.availableProcessors(),
      Runtime.getRuntime.availableProcessors() * 2,
      30, TimeUnit.SECONDS,
      new LinkedBlockingDeque(),
      daemonFactory,
      rejectedExecutionHandler))

  lazy val daemonFactory: ThreadFactory = runnable => {
    val t = new Thread(runnable)
    t.setDaemon(true)
    t
  }
  lazy val rejectedExecutionHandler: RejectedExecutionHandler = (runnable, pool) => {}

  def runInBackground[T](f: => T): Future[T] = Future(wrapFatal(f))(backgroundContext)

  def wrapFatal[T](f: => T): T =
    try
      f
    catch {
      case e: VirtualMachineError =>
        throw new Exception(s"VM error '${e.getClass.getSimpleName}' caught (assuming it non-fatal)", e)
    }

}
