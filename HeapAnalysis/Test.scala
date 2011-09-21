class A {
  var n : A = null;

  def overwriteField = {
   val x = new A
   x.n = x
   x.n = null
 }
}