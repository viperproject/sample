package test.files

/**
 * Created with IntelliJ IDEA.
 * User: milos
 * Date: 6/5/13
 * Time: 4:41 PM
 * To change this template use File | Settings | File Templates.
 */
class Node {

  var value: Int = 0
//  var length: Int = 0
  var next: Node = null

  def createNext(v : Int) = {

//    if (this.value < 0) {
//      this.value = 2
//    }

//    var first = new Node()
//    first = null

//    var p = this
//    p.value = 0
//    p = p.next
//    if (p != null) {
//      p.value = 0
//      p = p.next
//    }


//    var p = this
//    while (p != null) {
//      p.value = 0
//      p.next = p
//    }

    // First nat
    var result : Node = null
    var p : Node = null
    var i = v
    while (i >= 0) {
      p = new Node()
      p.value = i
      p.next = result
      result = p
      p = null
      i = i - 1
    }

//    acyclic list
//    var result : Node = null
//    var p : Node = null
//    var i = 1
//    while (i <= v) {
//      p = new Node()
//      p.length = i
//      p.next = result
//      result = p
//      p = null
//      i = i + 1
//    }
//    p = result
//    while (p != null) {
//      p.value = 0
//      p = p.next
//    }
//    if (p != null) {
//      p.value = 0
//      p = p.next
//    }


//    value = 0
//    var p = this
//    p.next = new Node()
//    p = p.next
//    p.value = 0

//    value = 0
//    var p = this
//    var i = 1
//    while (i < v) {
//      p.next = new Node()
//      p = p.next
//      p.value = 0
//      i = i + 1
//    }

//    var it = this
//    var result: Node = null
//    while (it != null && it.value < v)
//      it = it.next
//    if (it != null && it.value == v)
//      result = it
//    else
//      result = null

//    value = 0
//    while (value == v)
//      next = new Node()
//
//    if (next != null)
//      next.value = v

//    var result = new Node()
//    var p = result
//    p.value = 0
//    var i = 1
//    while (i < v) {
//      p.next = new Node()
//      p = p.next
//      p.value = i
//      i = i + 1
//    }

//    value = 0
//    var p = this
//    var i = 1
//    var tmp: Node = null
//    while (i < v) {
//      tmp = new Node()
//      p.next = tmp
//      tmp = null
//      p = p.next
//      p.value = i
//      i = i + 1
//    }

//    var i = 0
//    var p = this
//    p.value = i
//    if (i < v ) {
//      p.value = i
//      p.next = new Node()
//      p = p.next
//      i = i + 1
//    } else {
//      p.value = v
//    }



//    value = 0
//    next = new Node()
//    next.value = v
//    next = null
//    var p = this
//    p = p.next
//    p = new Node()
//    if (v > 0)
//      p.value = v



//    var p = next
//    if (v > 0)
//      next.value = v
//    else
//      next.value = -v
////    next.value = value + 1
//    var p: Node = this
  }

}
