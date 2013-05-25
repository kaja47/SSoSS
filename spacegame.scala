package ssoss

import language.postfixOps
import com.vividsolutions.jts.geom
import geom.util.AffineTransformation

object Game {


// heartbeat functions

def tick(periodMillis: Int)(f: => Unit): Long = {
  val startTime = System.nanoTime
  f
  val endTime = System.nanoTime
  val sleepNanos = periodMillis * 1000000 - (endTime - startTime)
  if (sleepNanos >= 0) {
    Thread.sleep(sleepNanos / 1000000, sleepNanos % 1000000 toInt)
  } else {
    println("tick error: no sleep at all ("+sleepNanos+" ns)")
  }
  endTime - startTime
}

def makeHeartBeatThread(fps: Int)(f: => Any) =
  new Thread(new Runnable {
    val periodMillis = 1000 / fps
    def run: Unit = {
      var total    = 0L
      var measures = 0L
      while(true) { 
        total += tick(periodMillis)(f)
        measures += 1
        if (measures == 5 * fps) {
          printf("%f ms\n", 1.0 * total / measures / 1000000)
          total = 0
          measures = 0
        }
      }
    }
  })

}
