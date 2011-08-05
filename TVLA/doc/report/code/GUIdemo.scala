class A { 
  var i = 0; 
  var n: A = null
}

object SomeObject {
  def someMethod(unknown: Boolean) = {
  	var x,y : A = null 
  	if (unknown) {
  	  x = new A
  	  x.i = 1 
  	} else {
  	  y = new A
  	  y.i = 2
  	}
  }
}
