package ch.ethz.inf.pm.sample.util

object Timer {
	var start:Long = 0L
	var end:Long = 0L
	def go = {
		start = System.currentTimeMillis
  	}
  	def stop : String = {
		end = System.currentTimeMillis
		return ((end - start)/ 1000.0 + " s")
  	}
  	

}