package utils

/**
 * Created by IntelliJ IDEA.
 * User: chillipower_uk
 * Date: 02/01/2011
 * Time: 18:19
 * To change this template use File | Settings | File Templates.
 */
object Timer {

  /*
  def time[T](f: => T, msg : String = "Took %d ms to execute") : T = {
    val then = System.currentTimeMillis
    val res = f
    val now = System.currentTimeMillis
    Log.info(msg.format(now-then))
    res
  }
  */

  def time[T](timerFn : Long => Unit)(fn : => T) : T = {
    val start = System.currentTimeMillis
    val res = fn
    val now = System.currentTimeMillis
    timerFn(now - start)
    res
  }
}
