package ch.ethz.inf.pm.sample.abstractdomain.heapanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import scala.collection.mutable

/**
 * Implements reachability analysis of abstract heap locations in the non-relational heap domains of sample
 *
 * Given a variable environment V -> P(H) and a heap environment H -> P(H), this can compute non-reachable parts,
 * all reachable from a specific node and so on.
 *
 * This class contains old code that is most likely broken (marked @Deprecated). If you need it, fix it.
 *
 */
object ReachabilityAnalysis {

  def getUnreachableLocations[I <: NonRelationalHeapIdentifier[I]](env: VariableEnv[I], store: HeapEnv[I]): Set[I] = {
    val storeIds = store.ids
    val res = storeIds -- getReachableLocations(env, store, storeIds)
    res
  }

  def getReachableLocations[I <: NonRelationalHeapIdentifier[I]](env: VariableEnv[I], store: HeapEnv[I], storeIds: Set[I]): mutable.HashSet[I] = {
    if (env.isBottom) return mutable.HashSet.empty[I]
    if (store.isBottom) return mutable.HashSet.empty[I]

    val toVisit = mutable.ListBuffer.empty[I]
    val reachable = mutable.HashSet.empty[I]

    val reachableViaFieldAccessEtc = store.getReachableMap

    for (set <- env.map.values) {
      toVisit ++= set.value
      reachable ++= set.value
    }

    while (toVisit.nonEmpty) {
      val cur = toVisit.remove(0)
      for (reach <- store.get(cur).value) {
        if (!reachable.contains(reach)) {
          reachable += reach
          toVisit += reach
        }
      }
      reachableViaFieldAccessEtc.get(cur) match {
        case Some(xs) =>
          for (x <- xs) {
            if (!reachable.contains(x)) {
              reachable += x
              toVisit += x
            }
          }
        case None => ()
      }
    }

    reachable
  }

  def reach[I <: NonRelationalHeapIdentifier[I]](to: I, env: VariableEnv[I], store: HeapEnv[I]): (List[String], Boolean) = {
    @Deprecated
    var result: List[String] = Nil
    var b: Boolean = false
    for (id <- env.ids) {
      if (id.isInstanceOf[VariableIdentifier]) {
        reachable(id, to, env, store) match {
          case (l, true) =>
            if (result.size == 0 || l.size < result.size) {
              result = l
              b = true
            }
          case _ =>
        }
      }
    }
    (result, b)
  }

  /** most likely broken */
  @Deprecated
  def reachable[I <: NonRelationalHeapIdentifier[I]](from: Identifier, to: I, env: VariableEnv[I], store: HeapEnv[I]): (List[String], Boolean) = {
    if (from.equals(to)) return (Nil, false)
    reachable1(from, to, env, store)
  }

  /** most likely broken */
  @Deprecated
  private def reachable1[I <: NonRelationalHeapIdentifier[I]](from: Identifier, to: I, env: VariableEnv[I], store: HeapEnv[I]): (List[String], Boolean) = {
    val considered = scala.collection.mutable.Set.empty[I]
    from match {
      case x: VariableIdentifier => //It can be only as first step, so we removed the t.toString, it will be replaced by "this"
        for (hi <- env.get(x).value) {
          reachable(hi, to, env, store) match {
            case (path, true) => return ( /*x.toString::*/ path, true)
            case _ =>
          }
          for (field <- from.typ.possibleFields)
            reachable(new FieldAndProgramPoint(hi.asInstanceOf[ProgramPointHeapIdentifier], field.getName, field.typ), to, env, store) match {
              case (path, true) => return ( /*x.toString::*/ field.getName :: path, true)
              case _ =>
            }
        }
        (Nil, false)

      case x: I =>
        if (considered.contains(x)) {
          return (Nil, false)
        }
        if (x.equals(to)) return (Nil, true)
        val res = store.get(x).value
        if (res.contains(to)) return (Nil, true)
        //for(resSingle <- res) 
        if (x.isNormalized())
          ReachabilityAnalysis.isAccessibleThroughField(x, to, env, store) match {
            case Some(s) =>
              return (s :: Nil, true)
            case None =>
          }
        considered += x
        for (hi <- res) {
          reachable(hi, to, env, store) match {
            case (path, true) => return (path, true)
            case _ =>
          }
        }
        (Nil, false)
    }
  }

  /** most likely broken */
  @Deprecated
  private def isAccessibleThroughField[I <: NonRelationalHeapIdentifier[I]](from: Identifier, to: I, env: VariableEnv[I], store: HeapEnv[I]): Option[String] = {
    for (field <- from.typ.possibleFields) {
      if (from.isInstanceOf[I] && from.asInstanceOf[I].extractField(from.asInstanceOf[I], field.getName, field.typ).equals(to)) return Some(field.getName)
    }
    None
  }


}