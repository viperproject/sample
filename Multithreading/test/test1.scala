class C {
/*  def thread1(c : Cell) {
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
  }*/
  def thread5(c : Cell) {
    if(c.value > 1) {
      while(c.value1 > 1)
        if(c.value > 10)
          while(c.value1> 10)
            0;
      1;
    }
  }
}

class Cell {
  var value=0;
  var value1=0;
}