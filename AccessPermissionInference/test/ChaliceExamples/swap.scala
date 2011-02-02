package ChaliceExamples

class C {
  var F : Any = null;
  var G : Any = null;
  def n()
    //requires acc(F) && acc(this.G);
    //ensures acc(F) && acc(G);
  {
    var tmp = F;
    F = G;
    G = tmp;
  }
}
