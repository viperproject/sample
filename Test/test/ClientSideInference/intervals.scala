abstract class Intervals {
  def ex1(i : Int) : Int = {
    assert(i>=1)
    return i+1
    //    val result2 = ex2(j);
//    val result3 = ex2(k);
  }

  def ex2(iex2 : Int, jex2 : Int) = iex2

  def fibonacci(i : Int) : Int = {
    if(i==0)
      return 1
    else
      return fibonacci(i-1)+fibonacci(i-2)
  }

  def getRandom(v : Int) : Int

  def getRandom1() : Int

  def getRandom2() : Int
}