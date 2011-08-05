class A { var n: A = null }

object SomeObject {
  def someMethod = {
    val x = new A
    x.n = new A
  }
}
