package Examples.Chalice2

class C {
  var F : Any;
  var G : Any;
  def n()
    //requires acc(F) && acc(this.G);
    //ensures acc(F) && acc(G);
  {
    var tmp = F;
    F = G;
    G = tmp;
  }
}
