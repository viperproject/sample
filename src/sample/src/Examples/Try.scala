package Examples

object Try {
  def main(args : Array[String]) : Unit = {System.out.println(foo("", "1"::"2"::"3"::Nil));}
  
  private def foo(exitState : String, block : List[String]) : List[String] = block match {
    case Nil => exitState :: Nil
    case x : List[String] => foo(x.last, x.dropRight(1)) ::: (exitState :: Nil)
  }
  
}
