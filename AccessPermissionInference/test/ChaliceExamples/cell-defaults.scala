package Examples.Chalice2

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

// verify this program with -defaults -autoFold -autoMagic

class CellDefault {
  var x: Int = 0;

  def init(v: Int) =
    //requires acc(this.x)
    //ensures valid
  {
    x = v;
  }

  def set(v: Int) =
    //requires valid
    //ensures valid
  {
    x = v;
  }
  
  def increment() =
    //requires valid;
    //ensures valid;
  {
    x = x + 1;
  }

  def dispose() =
    //requires valid
  {
    Chalice.free(this);
  }

  def get(): Int = 
    //requires valid;
  	//ensures valid;
  {
    x
  }

  //predicate valid {
  //  acc(this.x)
  //}

  //invariant valid;
}

class IntervalDefault {
  var left: CellDefault = null;
  var right: CellDefault = null;

  def init(l: Int, r: Int) =
    //requires acc(left) && acc(right);
    //ensures valid;
  {
    left = new CellDefault;
    left.init(l);
    right = new CellDefault;
    right.init(r);
  }

  def setLeft(l: Int) =
    //requires valid;
    //ensures valid;
  {
    left.set(l);
  }

  def setRight(r: Int) =
    //requires valid;
    //ensures valid;
  {
    right.set(r);
  }

  def shift(v: Int) =
    //requires valid;
    //ensures valid;
  {
    left.set(left.get()/*+v*/);
    right.set(right.get()/*+v*/);
  }

  def getLeft() : Int = 
    //requires valid;
	//ensures valid
  {
    left.get()
  }

  def getRight() : Int =
    //requires valid;
	//ensures valid;
  {
    right.get()
  }
  
  //predicate valid
  //{
  //  left.valid && right.valid
  //}
}

class ProgramDefault {
  def main(){
    var c1 = new CellDefault;
    c1.init(5);
    c1.set(6);

    var c2 = new CellDefault;
    c2.init(10);
    c2.set(11);

    //assert c1.get() == 6;
  }

  def main2(){
    var c: CellDefault = new CellDefault;
    c.init(0);
    c.dispose();

    //assert c.valid; // should fail
  }

  def main3() : CellDefault = 
    //ensures rt.valid
  {
    var rt = new CellDefault;
    rt.init(0)
    return rt;
  }
  /*def main4() = {
    var c: CellDefault = new CellDefault;
    c.init(0);
    Chalice.share(c);

    Chalice.acquire(c);
    c.set(1);
    Chalice.release(c);
  }*/
}