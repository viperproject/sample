class C {
  def thread1(c : Cell) {
    c.value=0;
    //c.value=1;
    c.value=c.value+1;
  }
  def thread2(c : Cell) {
    c.value=2;
    c.value=3;
    c.value=c.value+1;
  }
  def thread3(c : Cell) {
    c.value=0;
    c.value=c.value+1;
  }
  def thread4(c : Cell) {
    c.value=0;
    var i=0;
    while(i <= 5)
      c.value=c.value+1;
  }
}

class Cell {
  var value=0;
  var value1=0;
}