package ch.ethz.inf.pm.sample.oorepresentation


import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.util.Predef._
import ch.ethz.inf.pm.sample.oorepresentation.MethodIdentifier
import ch.ethz.inf.pm.sample.oorepresentation.VariableDeclaration
import ch.ethz.inf.pm.sample.oorepresentation.Variable
import ch.ethz.inf.pm.sample.AnalysisUnitContext
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier

/** Class element can be a method or a field. */
trait ClassElements

/** The identifier of a package. */
trait PackageIdentifier

/** Dummy package identifier. */
object DummyPackageIdentifier extends PackageIdentifier

/** The identifier of a class. */
trait ClassIdentifier {
  def getThisType(): Type
}

/** Dummy class identifier that just delegates to a type. */
case class DummyClassIdentifier(typ: Type) extends ClassIdentifier {
  def getThisType() = typ

  override def toString: String = typ.name
}

/** The identifier of a method. */
trait MethodIdentifier

/** Dummy method identifier that is just based on a string. */
case class DummyMethodIdentifier(name: String) extends MethodIdentifier {
  override def toString: String = name
}

/** A modifier of a field, parameters or method (e.g. static or abstract). */
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

case object StaticModifier extends Modifier

case object PureModifier extends Modifier

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
 * @param postcond the postconditions
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
class MethodDeclaration(
                         val programpoint: ProgramPoint,
                         val ownerType: Type,
                         val modifiers: List[Modifier],
                         val name: MethodIdentifier,
                         val parametricType: List[Type],
                         val arguments: List[List[VariableDeclaration]],
                         val returnType: Type,
                         val body: ControlFlowGraph,
                         val precond: Statement,
                         val postcond: Statement,
                         val classDef: ClassDefinition
                         ) extends ClassElements {

  override def toString: String =
    "method " +
      ToStringUtilities.toStringIfNotNull(returnType) + " " +
      name.toString +
      ToStringUtilities.parametricTypesToString(parametricType) +
      ToStringUtilities.listOfListToCommasRepresentation[VariableDeclaration](arguments) +
      "\n-------------------\nBODY:\n" +
      body.toString +
      "\n-------------------\n\n"

  def initializeArgument[S <: State[S]](state: S): S = {
    SystemParameters.semanticsComputing = false
    var result = state
    // Create a variable for the current object unless the method is static
    if (!modifiers.contains(StaticModifier)) {
      val thisVar = new Variable(programpoint,
        new VariableIdentifier("this", ownerType, programpoint))
      result = thisVar.forwardSemantics[S](result)
      val variable = result.getExpression
      result = result.removeExpression().createVariableForArgument(variable, ownerType)
    }
    // Create a variable for each formal parameter
    for (lv <- arguments) {
      for (variable <- lv) {
        result = variable.variable.forwardSemantics[S](result)
        val varExpr = result.getExpression
        result = result.removeExpression()
        result = result.createVariableForArgument(varExpr, variable.typ)
      }
    }
    result
  }

  /** this is not run by the touchdevelop code! */
  def forwardSemantics[S <: State[S]](state: S): ControlFlowGraphExecution[S] = {
    SystemParameters.withAnalysisUnitContext(AnalysisUnitContext(this)) {
      val result = initializeArgument[S](state)
      SystemParameters.semanticsComputing = true
      val r = new ControlFlowGraphExecution[S](body, state).forwardSemantics(result)
      SystemParameters.semanticsComputing = false
      r
    }
  }

  def backwardSemantics[S <: State[S]](state: S): ControlFlowGraphExecution[S] = {
    new ControlFlowGraphExecution[S](body, state).definiteBackwardSemantics(state)
  }

  //  def combinedSemantics[S <: State[S]](entrystate : S, exitstate : S) : ControlFlowGraphExecution[S] = {
  //    var result : S = initializeArgument[S](entrystate, arguments);
  //    new ControlFlowGraphExecution[S](body, entrystate).combinedSemantics(result, exitstate);
  //  }
}

/**
 * This class represents the declaration of a field.
 *
 * @param programpoint where the field is declared
 * @param modifiers the modifiers of the field
 * @param variable the name of the field
 * @param typ the type of the field
 * @param right the expression assigned to the field when it is initialized
 */
class FieldDeclaration(
                        override val programpoint: ProgramPoint,
                        val modifiers: List[Modifier],
                        override val variable: Variable,
                        override val typ: Type,
                        override val right: Option[Statement] = None)
  extends VariableDeclaration(programpoint, variable, typ, right) with ClassElements {

  override def toString: String =
    "field " +
      ToStringUtilities.toStringIfNotNull(typ) + variable.toString +
      ToStringUtilities.assignedIfNotNull(right)
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
                       val programpoint: ProgramPoint,
                       val typ: Type,
                       val modifiers: List[Modifier],
                       val name: ClassIdentifier,
                       val parametricTypes: List[Type],
                       val extend: List[ClassIdentifier],
                       var fields: List[FieldDeclaration],
                       var methods: List[MethodDeclaration],
                       val pack: PackageIdentifier,
                       val inv: Expression
                       ) {
  def addField(f: FieldDeclaration): Unit = fields = fields ::: f :: Nil

  def addMethod(m: MethodDeclaration): Unit = methods = methods ::: m :: Nil

  override def toString: String =
    ToStringUtilities.listToNewLineRepresentation[FieldDeclaration](fields) +
      "\n\n" +
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
class PackageDefinition(programpoint: ProgramPoint, name: PackageIdentifier, classes: List[ClassDefinition]) {
  override def toString: String = "package " + name + "\n\n" + ToStringUtilities.listToNewLineRepresentation[ClassDefinition](classes)
}

/**
 * This trait represents a type. It extends <code>Lattice</code> in order to represent the type hierarchy.
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
trait Type extends Lattice[Type] {
  require(isFloatingPointType implies isNumericalType)

  /** Returns `true` iff the current type is not a primitive type. */
  def isObject: Boolean

  /** Returns `true` iff the current type is a numerical type. */
  def isNumericalType: Boolean

  /** Returns `true` iff the current type represents a float type. */
  def isFloatingPointType: Boolean

  /** Returns `true` iff the current type represents a boolean type. */
  def isBooleanType: Boolean

  /** Returns `true` iff the current type represents a string type. */
  def isStringType: Boolean

  /** Returns `true` iff this type only represents one runtime instance. */
  def isStatic: Boolean

  /** Returns the name of the type. */
  def name: String

  /**
   * If the current type represents a class, it returns the list
   * of the possible fields, an empty set otherwise.
   */
  def possibleFields: Set[Identifier]

  /** Returns the possible fields with an object type. */
  def objectFields: Set[Identifier] =
    possibleFields.filter(_.getType.isObject)

  /** Returns the possible fields with a non-object type. */
  def nonObjectFields: Set[Identifier] =
    possibleFields.filter(!_.getType.isObject)

  /**
   * If the current type represents an array, it returns the type
   * of the elements contained in the array, None otherwise. 
   */
  def arrayElementsType: Option[Type]

  /**
   * This method returns `true` iff the current type can be only instance
   * of one of the given types and none else.
   * For instance, suppose that:
   * - the current type cannot be instantiated (e.g. it is an interface in Java of a trait in Scala)
   * - it cannot be extended by external libraries (e.g. it is declared as sealed in Scala)
   * - it is extended only by two classes C1 and C2 that cannot be extended
   *
   * If <code>types</code>={C1, C2}, then this method returns `true`
   */
  def isBottomExcluding(types: Set[Type]): Boolean
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
   * @param typeparameters the list of type generics
   * @param returnedtype the type of the returned value
   * @param state the abstract state in which the method call is evaluated
   * @return the abstract state obtained after the forward evaluation of the native method call, None if the semantics of the method call is not defined
   */
  def applyForwardNativeSemantics[S <: State[S]](thisExpr: ExpressionSet, operator: String, parameters: List[ExpressionSet], typeparameters: List[Type], returnedtype: Type, programpoint: ProgramPoint, state: S): Option[S];

  /**
   * It defines the backward semantics of native method calls
   *
   * @param thisExpr the expression representing the object on whom the method is called
   * @param operator the string of the called method
   * @param parameters the parameters of the called method
   * @param typeparameters the list of type generics
   * @param returnedtype the type of the returned value
   * @param programpoint the program point of the method call
   * @param state the abstract state in which the method call is evaluated
   * @return the abstract state obtained after the backward evaluation of the native method call, None if the semantics of the method call is not defined
   */
  def applyBackwardNativeSemantics[S <: State[S]](thisExpr: ExpressionSet, operator: String, parameters: List[ExpressionSet], typeparameters: List[Type], returnedtype: Type, programpoint: ProgramPoint, state: S, oldPreState: S): Option[S];
}

/** Native method semantics without backward semantics. */
trait ForwardNativeMethodSemantics extends NativeMethodSemantics {
  def applyBackwardNativeSemantics[S <: State[S]](
                                                   thisExpr: ExpressionSet,
                                                   operator: String,
                                                   parameters: List[ExpressionSet],
                                                   typeParameters: List[Type],
                                                   returnType: Type,
                                                   programPoint: ProgramPoint,
                                                   state: S,
                                                   oldPreState: S): Option[S] = None
}
