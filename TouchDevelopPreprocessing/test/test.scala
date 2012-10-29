/**
 *
 * Lucas Brutschy
 * Date: 9/24/12
 * Time: 6:47 PM
 *
 */


trait A {}
class C {}

val x = new C with A;



//trait Positional {
//  var pos:Int = 0
//  def copyTo(a:Positional):Positional = { a.pos = pos; a }
//}
//
//case class Node(s:String) extends Positional {
//  def copy(s: String = this.s): Node = { super.copyTo(new Node(s)).asInstanceOf[Node] }
//}
//
//val a = Node("a")
//a.pos = 3
//val b = a.copy(s = "b")
//println(b.pos)
//
//

