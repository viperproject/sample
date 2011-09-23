abstract class Intervals {
  def ex1(b1 : Int, b2 : Boolean) = {

    var i = getRandom(b1);
    //var j = getRandom1();
    //var k = getRandom2();
    while(b2)
      i=i-1;
    //k=k*2;
    //k=j*i;
  }

  def fibonacci(i : Int) : Int = {
    var result = 0;
    if(i==0)
      result=1;
    else
    {
      val j = i-1;
      result=fibonacci(j);
    }
    return result;
  }

  def getRandom(v : Int) : Int;
  def getRandom1() : Int;
  def getRandom2() : Int;
}