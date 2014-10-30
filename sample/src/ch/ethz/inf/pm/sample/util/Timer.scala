package ch.ethz.inf.pm.sample.util


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
		return ("%1.3f" format ((end - begin) / 1000.0)) + " s"
  	}
}


/**
 * Utility class for measuring accumulated time
 *
 * @author Lucas Brutschy
 *
 */
object AccumulatingTimer {

  case class TimeEntry(sum:Long, openStartTime:Option[Long])

  val times = new scala.collection.mutable.HashMap[String,TimeEntry]

  def start(s:String) {
    times.get(s) match {
      case None => times(s) = TimeEntry(0,Some(System.currentTimeMillis))
      case Some(TimeEntry(x,None)) => times(s) = TimeEntry(x,Some(System.currentTimeMillis))
    }
  }

  def stop(s:String) {
    times.get(s) match {
      case Some(TimeEntry(x,Some(y))) => times(s) = TimeEntry(x+(System.currentTimeMillis-y),None)
    }
  }

  def dump: String = {
    times.mkString("\n")
  }

}