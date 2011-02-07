package ch.ethz.inf.pm.sample.abstractdomain.accesspermissions

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._


trait LevelPermission {
  def maxPermission() : Int;
  def minPermission() : Int;
}

trait PermissionsDomain[P <: PermissionsDomain[P]] extends SemanticDomain[P] {
  def keys() : scala.collection.Set[Identifier];
  def get(id : Identifier) : LevelPermission;
  def inhale(id : Identifier, p : SymbolicValue) : P;
  def exhale(id : Identifier, p : SymbolicValue) : P;
  def free(id : Identifier) : P;
  def inhale(id : Identifier, p : Int) : P;
  def exhale(id : Identifier, p : Int) : P;
  def setPermissionLevel(id : Identifier, p : Int) : P;
}



class Path(val p : List[String]) {
  override def toString() : String = {
    var result : String ="";
    for(el <- p)
      if(result.equals("")) result=el;
      else result=result+"."+el;
    return result;
  }
  override def hashCode() = p.hashCode();
  override def equals(a : Any) : Boolean = a match {
    case x : Path =>
      if(p.size!=x.p.size) return false;
      for(i <- 0 to p.size-1)
        if(! p.apply(i).equals(x.p.apply(i)))
          return false;
      return true;
    case _ => return false;
  } 
}

sealed abstract class SymbolicValue(var path : Path) {
  def setPath(p : Path) : SymbolicValue = 
	  if(path==null) {
	 	  if(this==Epsilon)
	 	 	  throw new PermissionsException("Epsilon does not have a path");
	 	  else {
	 	 	  path=p;this
	 	  }
	  } else throw new PermissionsException("Path already initialized");
  def factory() : SymbolicValue; 
}

case object Epsilon extends SymbolicValue(null) { 
  override def toString() = "Epsilon";
  override def hashCode() = 0;
  override def equals(a : Any) : Boolean = super.equals(a)
  /*a match {
    case a : Epsilon => return true
    case _ => return false;
  }*/
  override def factory() : SymbolicValue = this;
}

case class SymbolicMonitorInvariant(val c : String, p : Path) extends SymbolicValue(p) { 
  override def toString() = "Invariant("+c.toString()+", "+path.toString()+")";
  override def hashCode() = c.hashCode();
  override def equals(a : Any) : Boolean = a match {
    case x: SymbolicMonitorInvariant => return c.equals(x.c) && path.equals(x.path);
    case _ => return false;
  }
  override def factory() : SymbolicValue = new SymbolicMonitorInvariant(c, p);
}

case class SymbolicAbstractPredicates(val c : String, val name : String, p : Path) extends SymbolicValue(p) { 
  override def toString() = "Predicate("+c.toString()+"."+name+", "+path.toString()+")";
  override def hashCode() = c.hashCode();
  override def equals(a : Any) : Boolean = a match {
    case x : SymbolicAbstractPredicates => return c.equals(x.c) && name.equals(x.name) && path.equals(x.path);
    case _ => return false;
  }
  override def factory() : SymbolicValue = new SymbolicAbstractPredicates(c, name, p);
}

case class SymbolicPreCondition(val className : String, val methodName : String, p : Path) extends SymbolicValue(p) { 
  override def toString() = "pre("+className.toString()+"."+methodName.toString()+", "+path.toString()+")";
  override def hashCode() = methodName.hashCode();
  override def equals(a : Any) : Boolean = a match {
    case x : SymbolicPreCondition => className.equals(x.className) && methodName.equals(x.methodName) && path.equals(x.path);
    case _ => return false;
  }
  override def factory() : SymbolicValue = new SymbolicPreCondition(className, methodName, p);
}

case class SymbolicPostCondition(val className : String, val methodName : String, p : Path) extends SymbolicValue(p) { 
  override def toString() = "post("+className.toString()+"."+methodName.toString()+", "+path.toString()+")";
  override def hashCode() = methodName.hashCode();
  override def equals(a : Any) : Boolean = a match {
    case x : SymbolicPostCondition => className.equals(x.className) && methodName.equals(x.methodName) && path.equals(x.path);
    case _ => return false;
  }
  override def factory() : SymbolicValue = new SymbolicPostCondition(className, methodName, p);
}

sealed abstract class IntOrTop {
  def max(a: IntOrTop, b : IntOrTop) = a match {
    case Top => Top
    case WrappedInt(i) => b match {
      case Top => Top
      case WrappedInt(i2) => WrappedInt(Math.max(i, i2))
    }
  }
  
  def min(a: IntOrTop, b : IntOrTop) = a match {
    case Top => b
    case WrappedInt(i) => b match {
      case Top => a
      case WrappedInt(i2) => WrappedInt(Math.min(i, i2))
    }
  }
  
  def >=(b : IntOrTop) = this match {
    case Top => true
    case WrappedInt(i) => b match {
      case Top => false;
      case WrappedInt(i2) => i>=i2
    }
  }
  
  def +(b : IntOrTop) : IntOrTop = this match {
    case Top => Top
    case WrappedInt(i) => b match {
      case Top => Top;
      case WrappedInt(i2) => WrappedInt(i+i2)
    }
  }
  
  def -(b : IntOrTop) : IntOrTop = this match {
    case Top => Top
    case WrappedInt(i) => b match {
      case Top => Top;
      case WrappedInt(i2) => WrappedInt(i-i2)
    }
  }
  
  override def toString() = this match {
    case Top => "Uknown"
    case WrappedInt(i) => i.toString();
  }
  
  override def hashCode() = this match {
    case Top => 0
    case WrappedInt(i) => i
  }
  
  
}
case object Top extends IntOrTop 

case class WrappedInt(val i : Int) extends IntOrTop {
  override def equals(a : Any) : Boolean = a match {
    case WrappedInt(i1) => return i==i1;
    case _ => return false;
  }
}


class CountedSymbolicValues(val n : IntOrTop, val s : SymbolicValue) {
  
  def this(i : Int) = this(WrappedInt(i), null);
  
  override def toString() = s match {
    case null => n.toString();
    case k => n.toString()+"*"+s.toString();
  }
  
  override def hashCode() = n.hashCode();
  
  override def equals(a : Any) : Boolean = a match {
    case b : CountedSymbolicValues => return n.equals(b.n) && ((b.s==null && s==null) || s.equals(b.s));
    case _ => return false;
  }
  
  def sameSymbolicValue(a : CountedSymbolicValues) : Boolean = {
    if(s==null && a.s==null) return true;
    if(s==null || a.s==null) return false;
    this.s.equals(a.s);
  }
    
  def lub(a : CountedSymbolicValues, b : CountedSymbolicValues) = {
    assert(a.sameSymbolicValue(b))
    new CountedSymbolicValues(a.n.min(a.n, b.n), a.s);
  }
  
  def glb(a : CountedSymbolicValues, b : CountedSymbolicValues) = {
    assert(a.sameSymbolicValue(b))
    new CountedSymbolicValues(a.n.max(a.n, b.n), a.s);
  }
  
  def +(b : CountedSymbolicValues) = {
    assert(this.sameSymbolicValue(b))
    new CountedSymbolicValues(this.n+b.n, this.s);
  }
  
  def -(b : CountedSymbolicValues) = {
    assert(this.sameSymbolicValue(b))
    new CountedSymbolicValues(this.n-b.n, this.s);
  }
}


class SymbolicLevelPermission() extends Lattice[SymbolicLevelPermission] with LevelPermission {
  var value : Set[CountedSymbolicValues] = Set.empty[CountedSymbolicValues]
  var isBottom : Boolean = false; 
  def this(s : Set[CountedSymbolicValues]) = {
    this();
    value=s;
  }
  def this(el : CountedSymbolicValues) = {
    this();
    value=this.addElement(this.value, el);
  }
  
  private def addElement(s : Set[CountedSymbolicValues], el : CountedSymbolicValues) : Set[CountedSymbolicValues] = {
    var result=s;
    if(el.n.isInstanceOf[WrappedInt] && el.n.asInstanceOf[WrappedInt].i!=0)
    	result=result+el;
    return result;
  }
  
  override def toString() : String = {
    if(isBottom) return "_|_";
    if(value.isEmpty) return "0";
    var result : String = "";
    var first = true;
    for(el <- value)
      if(first) {
    	  result=el.toString();
    	  first=false;
      }
      else result = result+" + "+el.toString();
    return result;
  }
  
  override def hashCode() = value.hashCode();
  
  override def equals(a : Any) : Boolean = a match {
    case b : SymbolicLevelPermission => (this.isBottom && b.isBottom) || this.value.equals(b.value);
    case _ => return false;
  }
  
  override def minPermission() : Int = {
    for(el <- value)
      if(el.s==null) el.n match {
        case WrappedInt(i) => return i;
      }
    return 0;
  }
  
  override def maxPermission() : Int = {
    if(value.size==1) {
      val el = value.elements.next();
      if(el.s==null) el.n match {
        case WrappedInt(i) => return i;
      }
    }
    return 100;
  }
  
  override def glb(a : SymbolicLevelPermission, b : SymbolicLevelPermission) : SymbolicLevelPermission = {
    if(a.isBottom || b.isBottom) return bottom();
    var result : Set[CountedSymbolicValues] =  Set.empty[CountedSymbolicValues];
    for(e1 <- a.value) {
      var o : CountedSymbolicValues = null;
      for(e2 <- b.value)
        if(e2.sameSymbolicValue(e1)) o=e2;
      if(o==null)
        result=this.addElement(result, e1);
      else result=this.addElement(result, e1.glb(e1, o));
    }
    for(e1 <- b.value) {
      var o : CountedSymbolicValues = null;
      for(e2 <- a.value)
        if(e2.sameSymbolicValue(e1)) o=e2;
      if(o==null)
        result=this.addElement(result, e1);
    }
    new SymbolicLevelPermission(result);
  }
  
  override def widening(a : SymbolicLevelPermission, b : SymbolicLevelPermission)=this.lub(a, b);

  override def lub(a : SymbolicLevelPermission, b : SymbolicLevelPermission): SymbolicLevelPermission = {
    if(a.isBottom) return b;
    if(b.isBottom) return a;
    var result : Set[CountedSymbolicValues] =  Set.empty[CountedSymbolicValues];
    for(e1 <- a.value) {
      var o : CountedSymbolicValues = null;
      for(e2 <- b.value)
        if(e2.sameSymbolicValue(e1)) o=e2;
      if(o!=null)
        result=this.addElement(result, e1.lub(e1, o));
    }
    new SymbolicLevelPermission(result);
  }
  
  override def lessEqual(a : SymbolicLevelPermission) : Boolean = {
    if(this.isBottom) return true;
    if(a.isBottom) return false;
    for(e <- a.value) {
      var ok : Boolean = false;
      for(e1 <- this.value)
        if(e.sameSymbolicValue(e1) && e1.n>=e.n)
          ok=true;
      if(! ok) return false;
    }
    return true;
  }
    
    
  override def top() : SymbolicLevelPermission=new SymbolicLevelPermission();
  override def bottom() : SymbolicLevelPermission= {
    val result=new SymbolicLevelPermission();
    result.isBottom=true;
    return result;
  }
  override def factory() = this.top();
  
  def +(v : CountedSymbolicValues) : SymbolicLevelPermission= {
    var result : Set[CountedSymbolicValues] = Set.empty[CountedSymbolicValues];
    var added = false;
    for(el <- this.value) {
      if(v.sameSymbolicValue(el)) {
        added=true;
        result=this.addElement(result, v+el);
      }
      else result=this.addElement(result, el);
    }
    if(! added) result=this.addElement(result, v);
    return new SymbolicLevelPermission(result)
  }
  
  def ++(v : SymbolicLevelPermission) : SymbolicLevelPermission= {
    var result : Set[CountedSymbolicValues] = Set.empty[CountedSymbolicValues];
    for(el <- this.value) {
      var added=false;
      for(el1 <- v.value)
        if(el.sameSymbolicValue(el1)) {
          added=true;
          result=this.addElement(result, el+el1);
        }
      if(! added) result=this.addElement(result, el);
    }
    for(el <- v.value) {
      var added=false;
      for(el1 <- this.value)
        if(el.sameSymbolicValue(el1))
          added=true;
      if(! added) result=this.addElement(result, el);
    }
    return new SymbolicLevelPermission(result)
  }
  
  def -(v : CountedSymbolicValues) : SymbolicLevelPermission= {
    var result : Set[CountedSymbolicValues] = Set.empty[CountedSymbolicValues];
    var added = false;
    for(el <- this.value) {
      if(v.sameSymbolicValue(el)) {
        added=true;
        result=this.addElement(result, el-v);
      }
      else result=this.addElement(result, el);
    }
    if(! added) v.n match {
      case Top => throw new PermissionsException("I cannot subtract a potentially unbounded number of symbolic values")
      case WrappedInt(i) => result=this.addElement(result, new CountedSymbolicValues(new WrappedInt(-i), v.s));
    }
    return new SymbolicLevelPermission(result)
  }
  
  def --(v : SymbolicLevelPermission) : SymbolicLevelPermission= {
    var result : Set[CountedSymbolicValues] = Set.empty[CountedSymbolicValues];
    for(el <- this.value) {
      var added=false;
      for(el1 <- v.value)
        if(el.sameSymbolicValue(el1)) {
          added=true;
          result=this.addElement(result, el-el1);
        } 
      if(! added) result=this.addElement(result, el);
    }
    for(el <- v.value) {
      var added=false;
      for(el1 <- this.value)
        if(el.sameSymbolicValue(el1))
          added=true;
      if(! added) throw new PermissionsException("If I subtract a permission I should have it in the abstract state");
    }
    return new SymbolicLevelPermission(result)
  }
  
}

class SymbolicPermissionsDomain[I <: NonRelationalHeapIdentifier[I]] extends BoxedDomain[SymbolicLevelPermission, SymbolicPermissionsDomain[I]] with PermissionsDomain[SymbolicPermissionsDomain[I]] with AddressedDomain[I]{

  def keys() = value.keySet;
  
  def getAddresses() : Set[I] = {
    var result : Set[I] = Set.empty[I];
    for(id <- this.keys())
      if(id.isInstanceOf[HeapIdAndSetDomain[I]]) result=result++id.asInstanceOf[HeapIdAndSetDomain[I]].value; 
      else if(id.isInstanceOf[I]) result=result+id.asInstanceOf[I];//let's hope... (thanks Java erasure!!!)
    return result;
  }

  override def add(key : Identifier, value : SymbolicLevelPermission) : SymbolicPermissionsDomain[I] = {
    if(key.isInstanceOf[HeapIdAndSetDomain[I]]) {
      var result=this;
      for(add <- key.asInstanceOf[HeapIdAndSetDomain[I]].value)
        result=result.add(add, value);
      return result;
    }
    else return super.add(key, value);
  } 
  
  private def inhale(id : Identifier, p : CountedSymbolicValues) : SymbolicPermissionsDomain[I] = {
	val actual = this.get(id);
	if(! id.getType().toString().equals("Chalice") && ! id.isInstanceOf[VariableIdentifier]) ConstraintsInference.addConstraint(new Geq(Settings.writeLevel, new Add(ConstraintsInference.convert(p), ConstraintsInference.convert(actual))));
	if(! Settings.unsoundInhaling && ! id.representSingleVariable) //In order to be sound, I cannot inhale on heap summary nodes 
		return this;
	if(actual.equals(this.top()))
		return this.add(id, new SymbolicLevelPermission(p));
	else return this.add(id, actual+p);
  };
  private def exhale(id : Identifier, p : CountedSymbolicValues) : SymbolicPermissionsDomain[I] = {
	val actual = this.get(id);
	if(! id.getType().toString().equals("Chalice") && ! id.isInstanceOf[VariableIdentifier]) ConstraintsInference.addConstraint(new Geq(ConstraintsInference.convert(actual), ConstraintsInference.convert(p)));
	if(actual.equals(this.top()))
		return this.add(id, new SymbolicLevelPermission(p));
	else return this.add(id, actual-p);
  };
  
  override def inhale(id : Identifier, p : Int) : SymbolicPermissionsDomain[I] = this.inhale(id, new CountedSymbolicValues(p));
  override def exhale(id : Identifier, p : Int) : SymbolicPermissionsDomain[I] = this.exhale(id, new CountedSymbolicValues(p));
  
  override def free(id : Identifier) : SymbolicPermissionsDomain[I] =
    if(id.isInstanceOf[HeapIdAndSetDomain[I]]) {
      var result=this;
      for(add <- id.asInstanceOf[HeapIdAndSetDomain[I]].value) {
    	//the permission should be 100%
    	ConstraintsInference.addConstraint(new Eq(Settings.writeLevel, ConstraintsInference.convert(this.get(add))));
        result=result.remove(add);
      }
      return result;
    }
    else return super.remove(id);
  
  override def inhale(id : Identifier, p : SymbolicValue) : SymbolicPermissionsDomain[I] = this.inhale(id, new CountedSymbolicValues(new WrappedInt(1), p));
  
  override def exhale(id : Identifier, p : SymbolicValue) : SymbolicPermissionsDomain[I] = this.exhale(id, new CountedSymbolicValues(new WrappedInt(1), p));
  
  //private def inhale(id : Identifier, p : SymbolicLevelPermission) : SymbolicPermissionsDomain[I] = this.add(id, this.get(id)++p)
  
  //private def exhale(id : Identifier, p : SymbolicLevelPermission) : SymbolicPermissionsDomain[I] = this.add(id, this.get(id)--p)
  
  def get(variable : Identifier) : SymbolicLevelPermission = this.value.get(variable) match {
    case Some(x) => x;
    case None => 
      if(variable.isInstanceOf[HeapIdAndSetDomain[I]]) {
	      var result = new SymbolicLevelPermission().bottom();
	      for(id <- variable.asInstanceOf[HeapIdAndSetDomain[I]].value)
	        this.value.get(id) match {
	          case Some(y) => result=result.lub(result, y);
	          case None => return new SymbolicLevelPermission().top();
	          }
	      return result;
      }
      else return return new SymbolicLevelPermission().top();
  }
  
  def setPermissionLevel(variable : Identifier, l : Int) : SymbolicPermissionsDomain[I] = this.add(variable, new SymbolicLevelPermission(new CountedSymbolicValues(l)));
  
  def setToTop(variable : Identifier) : SymbolicPermissionsDomain[I] = this
  
  def removeVariable(variable : Identifier) : SymbolicPermissionsDomain[I] = this.remove(variable);
  
  def assign(variable : Identifier, expr : Expression) : SymbolicPermissionsDomain[I] = {
    if(! variable.getType().toString().equals("Chalice") && ! variable.isInstanceOf[VariableIdentifier])
    	ConstraintsInference.addConstraint(new Eq(Settings.writeLevel, ConstraintsInference.convert(this.get(variable))));
    return this
  }

  def backwardAssign(variable : Identifier, expr : Expression) : SymbolicPermissionsDomain[I] = throw new PermissionsException("Backward analysis not yet supported");
  
  def setParameter(variable : Identifier, expr : Expression) : SymbolicPermissionsDomain[I] = this
  
  def access(field : Identifier) : SymbolicPermissionsDomain[I] = {
    if(! field.getType().toString().equals("Chalice") && ! field.isInstanceOf[VariableIdentifier]) 
    	ConstraintsInference.addConstraint(new Geq(ConstraintsInference.convert(this.get(field)), Settings.readLevel));
    return this
    }
  
  def backwardAccess(field : Identifier) : SymbolicPermissionsDomain[I] = throw new PermissionsException("Backward analysis not yet supported");
  
  def assume(expr : Expression) : SymbolicPermissionsDomain[I] = this
  def createVariable(variable : Identifier, typ : Type) : SymbolicPermissionsDomain[I] = return this.setPermissionLevel(variable, 100);
  def createVariableForParameter(variable : Identifier, typ : Type, path : List[String]) : (SymbolicPermissionsDomain[I], Map[Identifier, List[String]]) = {
    var result = Map.empty[Identifier, List[String]];
    result=result+((variable, path ::: variable.toString() :: Nil))
    if(! variable.isInstanceOf[VariableIdentifier])
	    variable.getField() match {
		    case None => return (this.add(variable, new SymbolicLevelPermission(new CountedSymbolicValues(new WrappedInt(1), new SymbolicPreCondition(SystemParameters.currentClass.getName(), SystemParameters.currentMethod, new Path( path ::: Nil))))), result);
		    case Some(s) => return (this.add(variable, new SymbolicLevelPermission(new CountedSymbolicValues(new WrappedInt(1), new SymbolicPreCondition(SystemParameters.currentClass.getName(), SystemParameters.currentMethod, new Path( path  ::: /*s :: */Nil))))), result);
		  }
    else return (this, result);
  }
  def factory() : SymbolicPermissionsDomain[I] = new SymbolicPermissionsDomain[I]();
  
  
}

class PermissionsException(s : String) extends Exception(s)