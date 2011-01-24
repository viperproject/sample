package Examples
import scala.util.Random

object SelfContained {
  
  def main(args : Array[String]) : Unit = {
   var temp = new Peter();    
   var f : Int => Int = null;
   new Random nextInt(4) match {
     case 0 => f= (i : Int) => 2*i
     case 1 => f= (i : Int) => i*i
     case 2 => f= (i : Int) => if(i>=0) i else -i
     case 3 => f= (i : Int) => i-1
   }
   temp.foo(f, 0);
  }
}
