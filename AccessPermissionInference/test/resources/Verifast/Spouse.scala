package Verifast

object Chalice {

  def acquire(obj : Any) : Unit = {}
  def release(obj : Any) : Unit = {}
  def share(obj : Any) : Unit = {}
  def unshare(obj : Any) : Unit = {}
  def free(obj : Any) : Unit = {}
  def fold(objThis : Any, pred : String) : Unit = {}
  def unfold(objThis : Any, pred : String) : Unit = {}
  def fork(obj : Any, method : String) : Unit = {}
  def join(obj : Any, method : String) : Unit = {}
}

class Person(var spouse : Person) {

  //predicate valid
  //acc(this.spouse, 50) && acc(spouse.spouse, 50)

  //requires ?
  //ensures valid
  def init() = {
    Chalice.fold(this, "valid");
  }

  //requires valid
  //ensures valid
  def getSpouse() : Person = {
    Chalice.unfold(this, "valid");
    val result = spouse;
    Chalice.fold(this, "valid");
    return result;
  }

  //requires valid(this) && valid(other)
  //ensures valid(this) && valid(other)
  def marry(other : Person) = {
    Chalice.unfold(this, "valid");
    Chalice.unfold(other, "valid");
    this.spouse=other;
    other.spouse=this;
    Chalice.fold(this, "valid");
    Chalice.fold(other, "valid");
  }

  //requires valid(this) && valid(spouse)
  //ensures valid(this) && valid(spouse)
  def divorce(other : Person) = {
    Chalice.unfold(this, "valid");
    Chalice.unfold(spouse, "valid");
    spouse.spouse=null;
    this.spouse=null;
    Chalice.fold(this, "valid");
    Chalice.fold(spouse, "valid");
  }

}