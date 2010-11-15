package Examples


class PatternMatching {

  //Not exaustive but no warning! (not sound!)
/*  def ex1(x : Option[Int]) : Boolean = x match {
    case None => false
    case Some(5) => true
  }*/
  
  //Not exaustive but no warning! (not sound!)
  def ex2(x : List[Int]) : Boolean = x match {
    case Nil => false
    case 5 :: xs => true 
  }
  
  //Exaustive but warning! (not complete!)
  def ex3(x : List[C2]) : Boolean = x match {
    case x :: xs if(x.isInstanceOf[C2])=> true
    case Nil => false
  }
  
  //Exaustive but warning! (not complete!)
  //Unreachable code but no warning! (not sound!)
  def ex4(x : List[C2]) : Boolean = x match {
    case x :: xs if(x.isInstanceOf[C2])=> true
    case x :: xs if(x.isInstanceOf[C4])=> true
    case Nil => false
  }
  
}

sealed class C1
class C2 extends C1
class C3 extends C1
class C4 extends C2
