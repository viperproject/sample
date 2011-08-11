abstract class Intervals {
  def ex1(b : Boolean) = {
    var i = getRandom();
    var j = getRandom1();
    var k = getRandom2();
    while(b)
      i=i-1;
    if(b)
      k=i+j;
    else
      k=j+k;
    k = i + j + k;
  }
  def getRandom() : Int;
  def getRandom1() : Int;
  def getRandom2() : Int;
}