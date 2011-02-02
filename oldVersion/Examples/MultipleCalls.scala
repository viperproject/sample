package Examples
import scala.util.Random

object MultipleCalls {
  
  def main(args : Array[String]) : Unit = {
   var temp = new Peter();   
   def f(i : Int) : Int = if(i<=0) i+1; else g(i-1)
   def g(i : Int) : Int = if(i<=0) -1; else h(i-1)
   def h(i : Int) : Int = if(i<=0) 1; else f(i-1)
   temp.foo(f, 318);
  }
}
