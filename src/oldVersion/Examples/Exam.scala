package Examples

class Exam
class Mark(v : Int)
class Signature(p : Person)
class PassedExam(e : Exam, m : Mark, s : Signature)


trait Person {
	protected var name : String = "";
	def getName() : String=name;
}
trait Employee extends Person {
	protected var salary : Int =0;
	def getSalary() : Int=salary;
}
trait Teacher extends Person {
	protected var sig : Signature=null;
	def passExam(e : Exam, m : Int) : PassedExam = new PassedExam(e, new Mark(m), sig)
}

class Student(n : String) extends Person {
    this.name=n;
      
	var passedExams : List[PassedExam] = Nil;
 
 	def registerExam(e : PassedExam) : Unit = {
	  passedExams = e :: passedExams ;
   }
}

class Professor(n : String, s: Int) extends Teacher with Employee {
	this.name=n;
	this.salary=s;
	this.sig=new Signature(this);
}


class PhDStudent(name : String, s : Int) extends Student(name) with Employee {
	this.salary=s;
}


object Exam {
  def main(args : Array[String]) : Unit = {}
}



/*abstract class A[+T1, -T2] {
  def foo(e : T2) : T1;
}
class B
class C extends B

*/