package test.files


class SLNode {
  var next : SLNode = null
  var value : Int = 0
}

object SLLOfZeros {

  def createListOfZerosAndSum (k : Int) : (SLNode, Int) = {
    var list : SLNode = null
    var temp : SLNode = null
    var sum : Int = 0
    var i : Int = 0

    while (i < k) {
      temp = new SLNode()
      temp.value = 0
      temp.next = list
      list = temp
      i = i + 1
      temp = null
    }

    temp = list

    while (temp != null) {
      sum = sum + temp.value
      temp = temp.next
    }

    return (list, sum)
  }
}

object PartitionSLL {

  def partitionWithKey(list : SLNode, key : Int) : (SLNode, SLNode) = {
    var curr :  SLNode = list
    var smaller : SLNode = null
    var grThanOrEqual : SLNode = null
    var temp : SLNode = null

    while (curr != null) {
      if (curr.value < key) {
        temp = new SLNode()
        temp.value = curr.value
        temp.next = smaller
        smaller = temp
      } else {
        temp = new SLNode()
        temp.value = curr.value
        temp.next = grThanOrEqual
        grThanOrEqual = temp
      }
      curr = curr.next
      temp = null
    }
    return (smaller, grThanOrEqual)
  }
}

class SortedSLL {
  var head : SLNode = null

  def insertKey(key : Int) {
    var prev: SLNode = null
    var curr: SLNode = head
    var temp: SLNode = null

    while (curr != null && curr.value < key) {
      prev = curr
      curr = curr.next
    }

    temp = new SLNode()
    temp.value = key
    temp.next = curr

    if (prev == null)
      head = temp
    else
      prev.next = temp
  }

  def deleteKey(key : Int) {
    var prev: SLNode = null
    var curr: SLNode = head

    while (curr != null && curr.value < key) {
      prev = curr
      curr = curr.next
    }

    if (curr != null && curr.value == key)
      if (prev == null)
        head = curr.next
      else
        prev.next = curr.next
  }

  def deepCopy() : SortedSLL = {
    var curr : SLNode = head
    var last : SLNode = null
    val result : SortedSLL = new SortedSLL()
    var keyToCopy : Int = 0

    while (curr != null) {
      keyToCopy = curr.value
      if (result.head == null) {
        result.head = new SLNode()
        last = result.head
      } else {
        last.next = new SLNode()
        last = last.next
      }
      last.value = keyToCopy
      curr = curr.next
    }

    return result
  }

  def printSLL() {
    var curr: SLNode = head

    while (curr != null) {
      print(curr.value + " ")
      curr = curr.next
    }
  }

}

object TestSortedSLL {

  def main(args: Array[String]) {
    val list : SortedSLL = new SortedSLL()
    list.insertKey(2)
    list.insertKey(1)
    list.insertKey(3)
    list.insertKey(5)
    list.insertKey(-1)
    println()
    val (lt, gt) = PartitionSLL.partitionWithKey(list.head, 3);
    val ltList = new SortedSLL()
    ltList.head = lt
    ltList.printSLL()
    println()
    val gtList = new SortedSLL()
    gtList.head = gt
    gtList.printSLL()
    println()
    list.deleteKey(4)
    list.printSLL()
    println()
    val copyList = list.deepCopy()
    copyList.printSLL()
    println()
    val (zNode, zSum) = SLLOfZeros.createListOfZerosAndSum(5)
    val zList = new SortedSLL()
    zList.head = zNode
    zList.printSLL()
    println()
    print(zSum)
  }

}
