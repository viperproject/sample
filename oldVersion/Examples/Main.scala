package Examples
import scala.util.Random

object Main {
  
  def main(args : Array[String]) : Unit = {
   var temp = new Peter();    
   var f : Int => Int = null;
   new Random nextInt(4) match {
     case 0 => f=temp.double;
     case 1 => f=temp.square;
     case 2 => f=temp.abs;
     case 3 => f=temp.decrement;
   }    
   temp.foo(f, 1);
  }
}
