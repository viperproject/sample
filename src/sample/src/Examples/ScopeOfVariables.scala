package Examples

object ScopeOfVariables {
  
  def temp() : Unit = {
    var i : Int = 0;
    def temp2() : Unit = {println(i);}
    if(true) {
      var i : Int = 10;
      temp2();
    }
  } 
  
  def main(args : Array[String]) : Unit = {
    temp();
  }
}
