package ch.ethz.inf.pm.sample.abstractdomain.heapanalysis

import ch.ethz.inf.pm.sample.abstractdomain._

object ReachabilityAnalysis {
//
//  def getAllReachableLocations[I <: NonRelationalHeapIdentifier[I]](env:VariableEnv[I],store:HeapEnv[I]) : Set[I] = {
//    var a = Set.empty[I]
//    for(id <- env.getIds) {
//      if(id.isInstanceOf[VariableIdentifier]) {
//        a = a ++ reachableFrom(id, a, store)
//      }
//    }
//  }
//
//  TODO
//
//  private def reachableFrom[I <: NonRelationalHeapIdentifier[I]](from : I, considered : Set[I], store : HeapEnv[I]) : Set[I] = {
//    var a = (considered ++ from.getAnnotatingNodes() ++ store.get(from).value)
//    for (id <- (from.getAnnotatingNodes() + from)) {
//      a = a ++ env.get()
//    }
//    store.get(from)
//  }

  def reach[I <: NonRelationalHeapIdentifier[I]](to : I, env : VariableEnv[I], store : HeapEnv[I]) : (List[String], Boolean)= {
	  var result : List[String] = Nil
	  var b : Boolean = false
	  for(id <- env.getIds) {
	 	  if(id.isInstanceOf[VariableIdentifier]) {
         reachable(id, to, env, store) match {
           case (l, true) =>
             if(result.size ==0 || l.size < result.size) {
               result=l
               b=true
             }
           case _ =>
         }
       }
	  }
	  (result, b)
  }

  /** most likely broken */
  def reachable[I <: NonRelationalHeapIdentifier[I]](from : Identifier, to : I, env : VariableEnv[I], store : HeapEnv[I]) : (List[String], Boolean)= {
    if(from.equals(to)) return (Nil, false)
	  reachable1(from, to, env, store)
  }

  /** most likely broken */
  private def reachable1[I <: NonRelationalHeapIdentifier[I]](from : Identifier, to : I, env : VariableEnv[I], store : HeapEnv[I]) : (List[String], Boolean)= {
    val considered=scala.collection.mutable.Set.empty[I]
    from match {
      case x : VariableIdentifier => //It can be only as first step, so we removed the t.toString, it will be replaced by "this"
        for(hi <- env.get(x).value) {
          reachable(hi, to, env, store) match {
            case (path, true) => return (/*x.toString()::*/path, true)
            case _ =>
          }
          for(field <- from.getType().getPossibleFields())
            reachable(new FieldAndProgramPoint(hi.asInstanceOf[ProgramPointHeapIdentifier], field.getName(), field.getType()), to, env, store) match {
              case (path, true) => return (/*x.toString()::*/field.getName()::path, true)
              case _ =>
            }
        }
        (Nil, false)

      case x : I =>
        if(considered.contains(x)) {return (Nil, false);}
        if(x.equals(to)) return (Nil, true)
        val res=store.get(x).value
        if(res.contains(to)) return (Nil, true)
        //for(resSingle <- res) 
        if(x.isNormalized())
          ReachabilityAnalysis.isAccessibleThroughField(x, to, env, store) match {
            case Some(s) =>
              return (s :: Nil, true)
            case None =>
          }
        considered+=x
        for(hi <- res) {
          reachable(hi, to, env, store) match {
            case (path, true) => return (path, true)
            case _ =>
          }
        }
        (Nil, false)
    }
  }

  /** most likely broken */
  private def isAccessibleThroughField[I <: NonRelationalHeapIdentifier[I]](from : Identifier, to : I, env : VariableEnv[I], store : HeapEnv[I]) : Option[String] = {
    for(field <- from.getType().getPossibleFields()) {
      if(from.isInstanceOf[I] && from.asInstanceOf[I].extractField(from.asInstanceOf[I], field.getName(), field.getType()).equals(to)) return Some(field.getName());
    }
    None
  }
  
	
}