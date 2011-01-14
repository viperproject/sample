package ch.ethz.inf.pm.sample.oorepresentation


import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain._
import scala.tools.nsc.symtab._

/** 
 * A class element can be a method or a field
 * 
 * @author Pietro Ferrara
 * @version 0.1
 */
trait ClassElements

/** 
 * The identifier of a package
 * 
 * @author Pietro Ferrara
 * @version 0.1
 */
trait PackageIdentifier

/** 
 * The identifier of a class
 * 
 * @author Pietro Ferrara
 * @version 0.1
 */
trait ClassIdentifier

/** 
 * The identifier of a method
 * 
 * @author Pietro Ferrara
 * @version 0.1
 */
trait MethodIdentifier

/** 
 * A modifier of a field, parameters or method (e.g. <code>static</code>, and <code>abstract</code>)
 * 
 * @author Pietro Ferrara
 * @version 0.1
 */
abstract class Modifier
case object CovariantModifier extends Modifier
case object ContravariantModifier extends Modifier
case object PrivateModifier extends Modifier
case object ProtectedModifier extends Modifier
case object VariableModifier extends Modifier
case object ArgumentModifier extends Modifier
case object AccessorModifier extends Modifier
case object OverrideModifier extends Modifier
case object AbstractModifier extends Modifier
case object DeferredModifier extends Modifier
case object CaseModifier extends Modifier
case object SealedModifier extends Modifier
case object FinalModifier extends Modifier
case object TraitModifier extends Modifier
case object ImplicitModifier extends Modifier
case object PublicModifier extends Modifier

/** 
 * This class represents the declaration of a method.
 * 
 * @param modifiers the modifiers of the declared method
 * @param name the name of the declared method
 * @param parametricType the values of generic types on which the method is parameterized
 * @param arguments the parameters of this method
 * @param returnType the type of the returned value
 * @param body the control flow graph representing the body of the method
 * @param precond the preconditions
 * @param precond the postconditions
 * @param precond the invariants
 * 
 * @author Pietro Ferrara
 * @version 0.1
 */
class MethodDeclaration(
                      programpoint : ProgramPoint, 
                      ownerType : Type,
                      modifiers : List[Modifier],
                      val name : MethodIdentifier, 
                      parametricType : List[Type], 
                      val arguments : List[List[VariableDeclaration]],
                      val returnType : Type,
                      val body : ControlFlowGraph,
                      val precond : Expression,
                      val postcond : Expression
              ) extends ClassElements 
{

  override def toString() : String = 
    "method "+
    ToStringUtilities.toStringIfNotNull(returnType)+" "+
    name.toString+
    ToStringUtilities.parametricTypesToString(parametricType)+
    ToStringUtilities.listOfListToCommasRepresentation[VariableDeclaration](arguments)+
    "\n-------------------\nBODY:\n"+
    body.toString()+
    "\n-------------------\n\n"
    
  
  
  private def initializeParameters[S <: State[S]](state : S, parameters : List[List[VariableDeclaration]]) : S = {
    SystemParameters.semanticsComputing=false;
    var result : S = state;
    result=new Variable(programpoint, new VariableIdentifier("this", ownerType)).forwardSemantics[S](result)
    val variable=result.getExpression();
    result=result.removeExpression().createVariableForParameter(variable, ownerType);
    for(lv <- parameters)
      for(variable <- lv) {
        result = variable.variable.forwardSemantics[S](result);
        val varExpr = result.getExpression();
        result=result.removeExpression();
        result=result.createVariableForParameter(varExpr, variable.typ);
      }
    return result;
  }
  
  def forwardSemantics[S <: State[S]](state : S) : ControlFlowGraphExecution[S] = {
    val result=initializeParameters[S](state, arguments);
    SystemParameters.semanticsComputing=true;
    val r=new ControlFlowGraphExecution[S](body, state).forwardSemantics(result)
    SystemParameters.semanticsComputing=false;
    return r;
  }
  
  def backwardSemantics[S <: State[S]](state : S) : ControlFlowGraphExecution[S] = {
    new ControlFlowGraphExecution[S](body, state).definiteBackwardSemantics(state)
  }
  
  def combinedSemantics[S <: State[S]](entrystate : S, exitstate : S) : ControlFlowGraphExecution[S] = {
    var result : S = initializeParameters[S](entrystate, arguments);
    new ControlFlowGraphExecution[S](body, entrystate).combinedSemantics(result, exitstate);
  }
}

/** 
 * This class represents the declaration of a method.
 * 
 * @param modifiers the modifiers of the field
 * @param name the name of the field
 * @param typ the type of the field
 * @param right the expression assigned to the field when it is initialized
 * 
 * @author Pietro Ferrara
 * @version 0.1
 */
class FieldDeclaration(
                      programpoint : ProgramPoint, 
                      modifiers : List[Modifier],
                      val name : Variable, 
                      typ : Type,
                      right : Statement
                      ) extends VariableDeclaration(programpoint, name, typ, right) with ClassElements {
  
  override def toString() : String = "field "+ToStringUtilities.toStringIfNotNull(typ)+name.toString+ToStringUtilities.assignedIfNotNull(right);
  
}

/** 
 * This class represents the declaration of a class.
 * 
 * @param modifiers the modifiers of the class
 * @param name the name of the class
 * @param parametricTypes the type on which the class is parameterized on
 * @param extend the list of the classes extended (note: it is a list in order to support multiple inheritance)
 * @param fields the list of the fields of the class
 * @param methods the list of the methods of the class
 * 
 * @author Pietro Ferrara
 * @version 0.1
 */
class ClassDefinition(
                      programpoint : ProgramPoint, 
                      modifiers : List[Modifier],
                      val name : ClassIdentifier,
                      parametricTypes : List[Type], 
                      extend : List[ClassIdentifier], 
                      var fields : List[FieldDeclaration], 
                      var methods : List[MethodDeclaration],
                      pack : PackageIdentifier,
                      val inv : Expression
                     )
{
  def addField(f : FieldDeclaration) : Unit = fields=fields ::: f :: Nil
  def addMethod(m : MethodDeclaration) : Unit = methods=methods ::: m :: Nil
  
  override def toString() : String = 
    ToStringUtilities.listToNewLineRepresentation[FieldDeclaration](fields)+
    "\n\n"+
    ToStringUtilities.listToNewLineRepresentation[MethodDeclaration](methods)

}

/** 
 * This class represents a package.
 * 
 * @param name the name of the package
 * @param classes the classes beloging to the package
 * 
 * @author Pietro Ferrara
 * @version 0.1
 */
class PackageDefinition(programpoint : ProgramPoint, name : PackageIdentifier, classes : List[ClassDefinition]) {
  override def toString() : String = "package "+name+"\n\n"+ToStringUtilities.listToNewLineRepresentation[ClassDefinition](classes)
} 

/** 
 * This trait represents a type. It extends <code>Lattice</code> in order to represent the type hierarchy.
 * 
 * @author Pietro Ferrara
 * @version 0.1
 */
trait Type extends Lattice[Type] {
  
  /**
   * This method returns <code>true</code> if and only if the current type is an object
   */
  def isObject() : Boolean;
  
  /**
   * This method returns <code>true</code> if and only if the current type is a numerical type
   */
  def isNumericalType() : Boolean;
  
  /**
   * This method returns <code>true</code> if and only if the current type is static, i.e. it represents only one runtime instance
   */
  def isStatic() : Boolean;
  
  /**
   * This method returns the name of the type
   */
  def getName() : String;
  
  /**
   * If the current type represents a class, it returns the list of the possible fields, an empty set otherwise. 
   */
  def getPossibleFields() : Set[(String, Type)];
  
  /**
   * This method returns <code>true</code> if and only if the current type can be only instance 
   * of one of the given types and none else.
   * For instance, suppose that:
   * - the current type cannot be instantiated (e.g. it is an interface in Java of a trait in Scala)
   * - it cannot be extended by external libraries (e.g. it is declared as sealed in Scala)
   * - it is extended only by two classes C1 and C2 that cannot be extended
   * 
   * If <code>types</code>={C1, C2}, then this method returns <code>true</code>
   */
  def isBottomExcluding(types : Set[Type]) : Boolean;
}

/** 
 * The semantics of the native methods.
 * Since we represent native operators (e.g. arithmetic operators, or dynamic type castings),
 * this class has to explain which is the semantics of such "native" method calls.
 * 
 * @author Pietro Ferrara
 * @version 0.1
 */
trait NativeMethodSemantics {
  
	  /**
	   * It defines the forward semantics of native method calls
	   * 
	   * @param thisExpr the expression representing the object on whom the method is called
	   * @param operator the string of the called method
	   * @param parameters the parameters of the called method
	   * @param listparameters the list of type generics 
	   * @param returnedtype the type of the returned value
	   * @param state the abstract state in which the method call is evaluated
	   * @return the abstract state obtained after the forward evaluation of the native method call, None if the semantics of the method call is not defined 
	   */
	def applyForwardNativeSemantics[S <: State[S]](thisExpr : SymbolicAbstractValue[S], operator : String, parameters : List[SymbolicAbstractValue[S]], typeparameters : List[Type], returnedtype : Type, state : S) : Option[S] ;
 
	  /**
	   * It defines the backward semantics of native method calls
	   * 
	   * @param thisExpr the expression representing the object on whom the method is called
	   * @param operator the string of the called method
	   * @param parameters the parameters of the called method
	   * @param listparameters the list of type generics
	   * @param returnedtype the type of the returned value
	   * @param state the abstract state in which the method call is evaluated
	   * @return the abstract state obtained after the backward evaluation of the native method call, None if the semantics of the method call is not defined
	   */
	def applyBackwardNativeSemantics[S <: State[S]](thisExpr : SymbolicAbstractValue[S], operator : String, parameters : List[SymbolicAbstractValue[S]], typeparameters : List[Type], returnedtype : Type, state : S) : Option[S] ;
}