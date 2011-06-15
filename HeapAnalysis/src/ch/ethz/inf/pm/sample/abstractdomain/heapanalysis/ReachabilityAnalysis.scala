package ch.ethz.inf.pm.sample.abstractdomain.heapanalysis

import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain._

object ReachabilityAnalysis {

	
  private var considered : Set[ProgramPointHeapIdentifier] = Set.empty[ProgramPointHeapIdentifier];
  
  def reach(to : ProgramPointHeapIdentifier, env : VariableEnv[ProgramPointHeapIdentifier], store : HeapEnv[ProgramPointHeapIdentifier]) : (List[String], Boolean)= {
	  var result : List[String] = Nil;
	  var b : Boolean = false;
	  for(id <- env.getIds) {
	 	  if(id.isInstanceOf[VariableIdentifier]) {
         reachable(id, to, env, store) match {
           case (l, true) =>
             if(result.size ==0 || l.size < result.size) {
               result=l;
               b=true;
             }
           case _ =>
         }
       }
	  }
	  return (result, b);
  }

  //It returns a path to go from "from" to "to". The second component of the pair is false iff "to" is not reachable from "from" 
  def reachable(from : Identifier, to : ProgramPointHeapIdentifier, env : VariableEnv[ProgramPointHeapIdentifier], store : HeapEnv[ProgramPointHeapIdentifier]) : (List[String], Boolean)= {
    if(from.equals(to)) return (Nil, false);
	  considered=Set.empty[ProgramPointHeapIdentifier];
	  reachable1(from, to, env, store);
  }
  
  private def reachable1(from : Identifier, to : ProgramPointHeapIdentifier, env : VariableEnv[ProgramPointHeapIdentifier], store : HeapEnv[ProgramPointHeapIdentifier]) : (List[String], Boolean)= from match {
    case x : VariableIdentifier => //It can be only as first step, so we removed the t.toString, it will be replaced by "this"
    	for(hi <- env.get(x).value) {
    		reachable(hi, to, env, store) match {
    		  case (path, true) => return (/*x.toString()::*/path, true);
    		  case _ =>
    		}
    		for(field <- from.getType().getPossibleFields())
    			reachable(new FieldAndProgramPoint(hi, field.getName(), field.getType()), to, env, store) match {
    			  case (path, true) => return (/*x.toString()::*/field.getName()::path, true);
    			  case _ =>
    			}
    	}
    	return (Nil, false);
     
    case x : ProgramPointHeapIdentifier =>
    	if(considered.contains(x)) {return (Nil, false);}
      	if(x.equals(to)) return (Nil, true);
        val res=store.get(x).value
        if(res.contains(to)) return (Nil, true);
        //for(resSingle <- res) 
        if(x.isNormalized)
          ReachabilityAnalysis.isAccessibleThroughField(x, to, env, store) match {
	          case Some(s) =>
	            return (s :: Nil, true);
	          case None =>
        }
    	considered+=x
	    for(hi <- res) {
	    	//if(!hi.equals(x))
	    		reachable(hi, to, env, store) match {
		    	  case (path, true) => return (path, true);
		    	  case _ =>
		    	}
	    }
        return (Nil, false);
    /*case x : MaybeHeapIdSetDomain[ProgramPointHeapIdentifier] =>
      if(x.value.contains(to)) return (Nil, true);
      for(k <- x.value) {
        val res=store.get(k).value
        if(res.contains(to)) return (Nil, true);
	      else
	    	for(hi <- res)
	    		reachable(hi, to, env, store) match {
	    		  case (path, true) => return (path, true)
	    		  case _ =>
	    		}
      }
      return (Nil, false);*/
  }
  
  private def isAccessibleThroughField[I <: NonRelationalHeapIdentifier[I]](from : Identifier, to : ProgramPointHeapIdentifier, env : VariableEnv[ProgramPointHeapIdentifier], store : HeapEnv[ProgramPointHeapIdentifier]) : Option[String] = {
    for(field <- from.getType().getPossibleFields()) {
      if(from.isInstanceOf[I] && from.asInstanceOf[I].extractField(from.asInstanceOf[I], field.getName(), field.getType()).equals(to)) return Some(field.getName());
    }
    return None;
  }
  
	
}