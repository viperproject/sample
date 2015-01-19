package ch.ethz.inf.pm.sample.util

object Relation {
  def empty[K,V]:Relation[K,V] = new Relation[K,V](Map.empty,Map.empty)
}

class Relation[K,V](forward:Map[K,Set[V]],backward:Map[V,Set[K]]) {

  def getAllLeft:Set[K] = forward.keySet
  def getAllRight:Set[V] = backward.keySet

  def getLeft(k:K):Option[Set[V]] = forward.get(k)
  def getRight(v:V):Option[Set[K]] = backward.get(v)

  def getLeftOrElse(k:K,v:Set[V]):Set[V] = forward.getOrElse(k,v)
  def getRightOrElse(v:V,k:Set[K]):Set[K] = backward.getOrElse(v,k)

  def add(k:K,v:V):Relation[K,V] = {
    val newForward = forward + (k -> (forward.getOrElse(k,Set.empty) + v ))
    val newBackward = backward + (v -> (backward.getOrElse(v,Set.empty) + k))
    new Relation(newForward,newBackward)
  }

  def add(k:K,vs:Set[V]):Relation[K,V] = vs.foldLeft(this)(_.add(k,_))
  def add(ks:Set[K],v:V):Relation[K,V] = ks.foldLeft(this)(_.add(_,v))

  def removeLeft(k:K):Relation[K,V]= {
    val values = forward.getOrElse(k,Set.empty)
    var newBackward = backward
    for (v <- values) {
      newBackward = newBackward + (v -> (newBackward.getOrElse(v,Set.empty) - k))
    }
    val newForward = forward - k
    new Relation(newForward,newBackward)
  }
  def removeRight(v:V):Relation[K,V]= {
    val keys = backward.getOrElse(v,Set.empty)
    var newForward = forward
    for (k <- keys) {
      newForward = newForward + (k -> (newForward.getOrElse(k,Set.empty) - v))
    }
    val newBackward = backward - v
    new Relation(newForward,newBackward)
  }

  def setLeft(k:K,v:V):Relation[K,V] = removeLeft(k).add(k,v)
  def setRight(k:K,v:V):Relation[K,V] = removeRight(v).add(k,v)

}
