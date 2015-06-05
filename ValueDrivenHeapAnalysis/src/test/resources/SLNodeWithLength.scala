package test.files

/**
 * Created with IntelliJ IDEA.
 * User: milos
 * Date: 10/24/13
 * Time: 9:11 AM
 * To change this template use File | Settings | File Templates.
 */
class SLNodeWithLength {
  var next : SLNodeWithLength = null
  var length : Int = 0
}


object CreateDLList {

  def createListOfLength(k : Int) : SLNodeWithLength = {
    var result : SLNodeWithLength = null
    var temp : SLNodeWithLength = null
    var i : Int = 1
    while (i <= k) {
      temp = new SLNodeWithLength()
      temp.next = result
      temp.length = i
      result = temp
      i = i + 1
      temp = null
    }
    result
  }

  def printList(l : SLNodeWithLength) {
    var curr : SLNodeWithLength = l
    while (curr != null) {
      print(curr.length + " ")
      curr = curr.next
    }
    println()
  }

  def main(args: Array[String]) {
    val l = createListOfLength(10)
    printList(l)
  }

}