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

class Account(var balance : Int) {
  //predicate valid
  //acc(this.balance)

  //requires acc(this.balance)
  //ensures valid
  def init() = {
    Chalice.fold(this, "valid")
  }

  //requires valid
  //ensures valid
  def getBalance() : Int = {
    Chalice.unfold(this, "valid")
    val result : Int = balance;
    Chalice.fold(this, "valid");
    return result;
  }

  //requires valid
  //ensures valid
  def deposit(amount : Int) = {
    Chalice.unfold(this, "valid")
    balance=balance+amount;
    Chalice.fold(this, "valid");
  }


  //requires valid && valid(target)
  //ensures valid && valid(target)
  def transferTo(target : Account, amount : Int) = {
    deposit(-amount);
    target.deposit(amount);
  }


}