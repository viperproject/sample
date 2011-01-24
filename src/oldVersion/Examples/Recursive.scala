package Examples
import scala.util.Random

object Recursive {
  
  def main(args : Array[String]) : Unit = {
   var temp = new Peter();    
   var f : Int => Int = null;
   def funct(i : Int) : Int = if(i<=0) 0; else funct(i-1)
   new Random nextInt(4) match {
     case 0 => f= funct
     case 1 => f= (i : Int) => i*i
     case 2 => f= (i : Int) => if(i>=0) i else -i
     case 3 => f= (i : Int) => i-1
   }
   temp.foo(f, 0);
  }
}
