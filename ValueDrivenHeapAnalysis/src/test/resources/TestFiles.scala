package test.files

/**
 * Created with IntelliJ IDEA.
 * User: milos
 * Date: 5/10/13
 * Time: 5:01 PM
 * To change this template use File | Settings | File Templates.
 */
class TestFiles {

  var fieldF : Int = 0

  def emptyMethod() = { }

  def valueVariableAssignment() = {
    var i : Int = 0
  }

}

class Node1 {

  var value: Int = 0
  var next: Node1 = null

  def findNode(x : Int) : Node1 = {
    var it = this
    var result: Node1 = null
    while (it != null && it.value < x)
      it = it.next
    if (it != null && it.value == x)
      result = it
    else
      result = null
    result
  }

}