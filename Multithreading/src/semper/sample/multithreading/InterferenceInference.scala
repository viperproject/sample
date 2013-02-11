package semper.sample.multithreading

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import numericaldomain._
import ch.ethz.inf.pm.sample.SystemParameters


object InterferenceInference {

   def extractAssignedValue[V <: SemanticDomain[V], S <: State[S], H <: HeapDomain[H, I], I <: HeapIdentifier[I]](cfgex : ControlFlowGraphExecution[S]) : ProgramPointAssignment[V] = {
     var result = new ProgramPointAssignment[V]();
     SystemParameters.currentCFG=cfgex.cfg;
     for(i <- 0 to cfgex.cfg.nodes.size-1) {
       val block = cfgex.cfg.nodes.apply(i);
       for(indexstatement <- 0 to block.size-1) {
         val statement = block.apply(indexstatement);
         if(statement.isInstanceOf[Assignment]) {
           val assignment = statement.asInstanceOf[Assignment];
           val state = assignment.left.forwardSemantics(cfgex.nodes.apply(i).apply(indexstatement));
           val assigned = state.getExpression();
           var assignedids : Set[Identifier] = Set.empty;
           for(e <- assigned.setOfExpressions)
             if(e.isInstanceOf[HeapIdSetDomain[I]])
               assignedids ++= e.asInstanceOf[HeapIdSetDomain[I]].value
           if(! assignedids.isEmpty)
             result = add(result, statement.getPC(), assignedids, cfgex.nodes.apply(i).apply(indexstatement + 1).asInstanceOf[AbstractState[FlowSensitivePartitioning[V], _, _]].getSemanticDomain)
         }
       }
     }
     SystemParameters.currentCFG=null;
     return result;
   }


  def add[I <: HeapIdentifier[I], H <: HeapDomain[H, I], S <: State[S], V <: SemanticDomain[V]](_result: ProgramPointAssignment[V], pc: ProgramPoint, assignedids: Set[Identifier], state: FlowSensitivePartitioning[V]): ProgramPointAssignment[V] = {
    var result: ProgramPointAssignment[V] = _result
    if(result.value.keySet.contains(pc))
      result = result.add(pc, result.get(pc).lub(result.get(pc), new AssignedIdentifiers[V](assignedids, state)))
    else result = result.add(pc, new AssignedIdentifiers[V](assignedids, state))
    result
  }
}
object ComputedInterference {
  //Thread1 -> pc1 -> x -> otherthreads -> 1 represents that Thread1 has written at program point pc1 value 1 to variable x when the other threads where at least at program points stored in otherthreads
  var value = new Interferences();
}

//A function representing the interferences exposed by each single thread
class Interferences[S <: SemanticDomain[S]] extends FunctionalDomain[String, ProgramPointAssignment[S], Interferences[S]] {
  def get(key: String): ProgramPointAssignment[S] = value.get(key) match {
    case None => new ProgramPointAssignment[S]().bottom();
    case Some(x) => x
  }
  def factory() = new Interferences[S];

}

//A function that for each program point tells the identifiers that are assigned and the abstract state after the assignment
class ProgramPointAssignment[S <: SemanticDomain[S]] extends FunctionalDomain[ProgramPoint, AssignedIdentifiers[S], ProgramPointAssignment[S]] {
  def get(key: ProgramPoint): AssignedIdentifiers[S] = value.get(key) match {
    case None => if(this.value.keySet.size>0)
        this.value.apply(this.value.keySet.head).bottom();
      else throw new SemanticException("Not supported")
    case Some(x) => x
  }
  def factory() = new ProgramPointAssignment[S];
}

//The identifiers that are assigned and the abstract state after the assignment
class AssignedIdentifiers[S <: SemanticDomain[S]](s : SetIdentifiers, v : FlowSensitivePartitioning[S]) extends CartesianProductDomain[SetIdentifiers, FlowSensitivePartitioning[S], AssignedIdentifiers[S]](s, v) {
  def this(s : Set[Identifier], v : FlowSensitivePartitioning[S]) = this(new SetIdentifiers(s), v)
  def factory() = new AssignedIdentifiers[S](s.bottom(), v.bottom());
}

class FlowSensitivePartitioning[S <: SemanticDomain[S]] extends FunctionalDomain[OtherThreads, S, FlowSensitivePartitioning[S]] with SemanticDomain[FlowSensitivePartitioning[S]] {
  def get(key: OtherThreads): S = value.get(key) match {
    case None => if(this.value.keySet.size>0)
      this.value.apply(this.value.keySet.head).bottom();
    else throw new SemanticException("Not supported")
    case Some(x) => x
  }
  def factory() = new FlowSensitivePartitioning[S];

  private def pointwiseApplication(semantic : S => S) : FlowSensitivePartitioning[S] = {
    if(this.isBottom) return this.bottom();
    var result=new FlowSensitivePartitioning[S];
    for(k <- this.value.keySet)
      result=result.add(k, semantic(this.value.apply(k)));
    return result;
  }

  private def empty() : Boolean = {
    if (this.isBottom || this.value.size==0) return true;
    else return false;
  }

  private def flowSensitiveApplication(semantic : S => S, expr : Expression) : FlowSensitivePartitioning[S] = {
    if(this.isBottom) return this.bottom();
    var result=new FlowSensitivePartitioning[S];
    val touchedIds = expr.identifiers();
    for(thread <- ComputedInterference.value.value.keySet)
      if(thread != SystemParameters.currentMethod)
        for(pc <- ComputedInterference.value.value.apply(thread).value.keySet){
          val ass = ComputedInterference.value.value.apply(thread).value.apply(pc);
          if(ass._1.value.intersect(touchedIds).size>0) {
            for(otherthreads <- ass._2.value.keySet) {
              if( ! (
                (otherthreads.value.keySet.contains(SystemParameters.currentMethod) &&
                  ( expr.getProgramPoint().getLine()==otherthreads.value.apply(SystemParameters.currentMethod).pc.get.getLine()
                  || SystemParameters.currentCFG.happensBefore(expr.getProgramPoint(), otherthreads.value.apply(SystemParameters.currentMethod).pc.get))
                  )
                ))
                result=result.add(otherthreads.add(thread, new ProgramPointLattice(pc)), semantic(ass._2.get(otherthreads)));
            }
          }
       }
    for(k <- this.value.keySet)
      result=result.add(k, semantic(this.value.apply(k)));
    return result;
  }

  def merge(f: Replacement) = if(empty) this else pointwiseApplication(_.merge(f))

  def setToTop(variable: Identifier) = if(empty) this else pointwiseApplication(_.setToTop(variable))

  def assign(variable: Identifier, expr: Expression) = if(empty) this else flowSensitiveApplication(_.assign(variable, expr), expr)

  def setArgument(variable: Identifier, expr: Expression) = if(empty) this else pointwiseApplication(_.setArgument(variable, expr))

  def assume(expr: Expression) = if(empty) this else flowSensitiveApplication(_.assume(expr), expr)

  def createVariable(variable: Identifier, typ: Type) = if(empty) this else pointwiseApplication(_.createVariable(variable, typ))

  //TODO: I'm ignoring the paths, maybe I should take them into account
  def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]) = if(empty) (this, Map.empty) else (pointwiseApplication(_.createVariableForArgument(variable, typ, path)._1), Map.empty)

  def removeVariable(variable: Identifier) = if(empty) this else pointwiseApplication(_.removeVariable(variable))

  def access(field: Identifier) = if(empty) this else pointwiseApplication(_.access(field))

  def backwardAccess(field: Identifier) = if(empty) this else pointwiseApplication(_.backwardAccess(field))

  def backwardAssign(variable: Identifier, expr: Expression) = if(empty) this else pointwiseApplication(_.backwardAssign(variable, expr))

  def getIds() = {
    var result : Set[Identifier] = Set.empty;
    for(s <- this.value.keySet)
      result=result++this.value.apply(s).getIds()
    result;
  }

  def getStringOfId(id: Identifier) = {
    var result : String = "";
    for(s <- this.value.keySet)
      result=result+s+" value "+this.value.apply(s).getStringOfId(id)+"\n";
    result;
  }
}

//A set of identifiers that are assigned
class SetIdentifiers(s : Set[Identifier]) extends SetDomain[Identifier, SetIdentifiers] {
  this.value=s;
  def factory() = new SetIdentifiers(Set.empty);

}

//Represent a lattice that stores information about program points.
//For now it is just a flat lattice with top and bottom
//In the future, I should add the order following the program order
//If a pc1 is after a pc2 in the program order, that it represents something more definite, so it is <=
class ProgramPointLattice() extends Lattice[ProgramPointLattice] {
  var pc : Option[ProgramPoint] = None;
  var isBottom = false;

  def this(pc : ProgramPoint) = {
    this();
    this.pc=Some(pc);
  }


  override def factory() =  new ProgramPointLattice();

  override def top() =  new ProgramPointLattice();

  override def bottom() = {
    val result = new ProgramPointLattice();
    result.isBottom=true;
    result;
  }

  override def lub(left: ProgramPointLattice, right: ProgramPointLattice): ProgramPointLattice = {
    if(left.isBottom && right.isBottom) return bottom();
    if(left.isBottom) return right;
    if(right.isBottom) return left;
    if(left.pc==None || right.pc==None) return top();
    if(left.pc.get.equals(right.pc.get)) return left;
    else return top();
  }

  override def glb(left: ProgramPointLattice, right: ProgramPointLattice): ProgramPointLattice =  {
    if(left.isBottom || right.isBottom) return bottom();
    if(left.pc==None) return right;
    if(right.pc==None) return left;
    if(left.pc.get.equals(right.pc.get)) return left;
    else return bottom();
  }

  override def widening(left: ProgramPointLattice, right: ProgramPointLattice) = lub(left, right);

  override def lessEqual(r: ProgramPointLattice) : Boolean = {
    if(this.isBottom) return true;
    if(r.isBottom) return false;
    if(r.pc==None) return true;
    if(this.pc==None) return false;
    if(r.pc.get.equals(this.pc.get)) return true;
    else return false;
  }

  override def equals(o : Any) : Boolean = o match {
    case p : ProgramPointLattice =>
      if(this.isBottom && p.isBottom) return true;
      if(this.isBottom || p.isBottom) return false;
      if(this.pc==None&& p.pc==None) return true;
      if(this.pc==None || p.pc==None) return false;
      return this.pc.get.equals(p.pc.get);
  }

  override def toString() : String = {
    if(isBottom) return "_|_"
    else if(pc==None) return "T"
    else return pc.get.toString
  }

  override def hashCode() : Int = {
    if (isBottom || pc==None) return 0;
    else return pc.get.hashCode()
  }
}

//A function that tracks where other threads are already surely arrived
class OtherThreads extends FunctionalDomain[String, ProgramPointLattice, OtherThreads]{
  def get(key: String): ProgramPointLattice = value.get(key) match {
    case None => new ProgramPointLattice().bottom();
    case Some(x) => x
  }
  def factory() = new OtherThreads;

}