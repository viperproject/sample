class AVP2012 {
 def example0(a : Int) = {
     var z : Int = 0;
   }

  def example1(a : Int) = {
      var i : Int = 0;
      if(a>0)
        i=a;
      else i=0-a;
    }


	def example2(a : Int, b : Int) = {
    var i : Int = 0;
    if(a%2==0)
      i=a;
    else i = a+1;
    if(a==0)
      i=2;
	}

	def example3() = {
		var i : Int = 0;
    var j : Int = 2;
    while(i < 100) {
      i=i+1;
      j=j*2;
    }
	}

	def example4(a : Int) = {
		var i : Int = 0;
    var j : Int = 2;
    while(i < 100) {
      if(a>0 && a%2==0) {
       j=j+a;
       i=i+1;
      }
    }
	}

	def example5(a : Int) = {
		var i : Int = 0;
    var j : Int = 2;
    while(i < a) {
      if(i==0)
        j=i*a+2;
      else if(i%2==1)
          j=j+a+1;
      else j=2*j+a;
      i=i+1
    }
	}

  def example6(j : Int) = {
    var result : Int =2;
    var i = 3;
    while(i<j) {
      result=result*i;
      i=i+1;
    }
  }

  def example7(j : Int) = {
    var result : Int = 0-2;
    var i = 0-3;
    while(j<i) {
      result=result*(0-i);
      i=i-1;
    }
  }

  def example8(j : Int) = {
    var a : Int = 1;
    var i : Int = j;
    while(j>=0) {
      if(j%2==1) {
        if(j>0)
          a=j;
        else
          a=0-j;
      }
      else if(j==0)  {
        if(j>0)
          a=j+1;
        else
          a=0-j-1;
      }
      i=i-1;
    }
  }

}
