class Cell {
  var x : String = "";
}

class StringList {
  var c : String="";
  var next : StringList = null;
}

class Test {
  def ex1(b : Boolean) : StringList = {
     var c : String = "a";
     var l : StringList = new StringList();
     l.c=c;
     var h = l;
     while(b) {
       l.next=new StringList();
       l=l.next;
       c="b"+c;
       l.c = c;
     }
     return h;
  }
}