class Test {
  def ex1(i : Int) = {
    var j = 5;
    if(i<10)
      j=1;
    else j=0-1;
  }

  def ex2(i : Int) = {
    var j = 5;
    var k = 0;
    if(i<10)
      j=1;
    else j=0-1;
    if(j==1)
      k=1;
    else k=0-1;
  }


  def ex3() = {
    var j = 49;
    var i : Int = 0;
    var k : Int = 1;
    while(i<=j) {
      i = i+1;
      k=i;
    }
  }

}