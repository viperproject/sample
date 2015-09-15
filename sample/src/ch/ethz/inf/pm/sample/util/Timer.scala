package ch.ethz.inf.pm.sample.util

import com.typesafe.scalalogging.LazyLogging


/**
 * Utility class for measuring time
 * 
 * @author Milos Novacek
 *
 */
object Timer {
	var begin:Long = 0L
	var end:Long = 0L
	
	def start = {
		begin = System.currentTimeMillis
  	}
  	
	def stop : String = {
		end = System.currentTimeMillis
		("%1.3f" format ((end - begin) / 1000.0)) + " s"
  	}
}


/**
 * Utility class for measuring accumulated time
 *
 * @author Lucas Brutschy
 *
 */
object AccumulatingTimer extends LazyLogging {

  case class TimeEntry(num:Long, sum:Long, openStartTime:Option[Long])

  val times = new scala.collection.mutable.HashMap[String,TimeEntry]

  def start(s:String) {
    times.get(s) match {
      case None => times(s) = TimeEntry(0, 0,Some(System.currentTimeMillis))
      case Some(TimeEntry(n,x,None)) => times(s) = TimeEntry(n,x,Some(System.currentTimeMillis))
      case Some(TimeEntry(n,x,Some(y))) =>
        logger.debug("Still had a running timer (did we crash?). Restarted!")
        times(s) = TimeEntry(n,x,Some(System.currentTimeMillis))
    }
  }

  def stop(s:String) {
    times.get(s) match {
      case Some(TimeEntry(n,x,Some(y))) => times(s) = TimeEntry(n+1,x+(System.currentTimeMillis-y),None)
      case _ => throw new RuntimeException("Stopping an entry that was never started")
    }
  }

  def stopAndWrite(s:String) {
    times.get(s) match {
      case Some(TimeEntry(n,x,Some(y))) =>
        val diff = (System.currentTimeMillis-y).toFloat/1000
        logger.info(s+" finished after "+f"$diff%2.2fs")
        times(s) = TimeEntry(n+1,x+(System.currentTimeMillis-y),None)
      case _ => throw new RuntimeException("Stopping an entry that was never started")
    }
  }

  override def toString: String = {
    times.map{ x =>
      val (name,TimeEntry(n,s,_)) = x
      val paddedName = name.padTo(40, ' ')
      val sInSeconds = s.toFloat/1000
      f"$paddedName%s\t$sInSeconds%2.2fs\t$n%s"
    }.toList.sorted.mkString("\n")
  }

  def reset() = times.clear()

}