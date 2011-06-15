class Cell {
  var x : Int = 0;
}

class Test {
  def ex1() = {
    val c1 : Cell = new Cell();
    val c2 : Cell = new Cell();
    c1.x=5;
    c2.x=0-5;
  }

  def ex2(i : Int) = {
    var c1 : Cell = new Cell();
    val c2 : Cell = new Cell();
    if(i>0)
      c1=c2;
    c1.x=5;
    c2.x=0-5;
  }

  def ex3() = {
    val c1 : Cell = new Cell();
    var i : Int = 0;
    c1.x=5;
    while(i<10) {
      i=i+1;
      c1.x=c1.x+i;
    }
  }
}