class Cell {
  var x : String = "";
}

class StringList {
  var c : String="";
  var next : StringList = null;
}

class Test {
  def ex1(i : Int, b : Boolean) = {
     //var j : Int = 0;
     var l : StringList = new StringList();
     l.c="a";
     //var h = l;
     if(b) {
       //l.next=new StringList();
       //l=l.next;
       l.c = "ab";
     }
     //return h;
  }
}