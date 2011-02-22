package ch.ethz.inf.pm.sample.util

object Timer {
	var begin:Long = 0L
	var end:Long = 0L
	def start = {
		begin = System.currentTimeMillis
  	}
  	def stop : String = {
		end = System.currentTimeMillis
		return ((end - begin)/ 1000.0 + " s")
  	}
  	

}