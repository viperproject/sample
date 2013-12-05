package graph.test

import graph._

/**
 * Created with IntelliJ IDEA.
 * User: milos
 * Date: 4/23/13
 * Time: 3:28 PM
 * To change this template use File | Settings | File Templates.
 */
object TestVertex {

  def main(args: Array[String]) {
    val v1 = LocalVariableVertex("a")(null)
    val v2 = LocalVariableVertex("b")(null)
    if (v1 < v2)
      println("v1 < v2")
    else
      println("v1 !< v2")

    val s1 : Option[String] = Some("str")
    val s2 : Option[String] = Some("str")

    if (s1.equals(s2)) println("s1 = s2")
    else println("s1 != s2")
  }

}
