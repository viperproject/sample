package ch.ethz.inf.pm.td.analysis

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{Constant, UnitExpression, VariableIdentifier, _}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.sample.reporting.Reporter
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.domain.{InvalidExpression, MultiValExpression}
import ch.ethz.inf.pm.td.semantics._

/**
 *
 * This class defines a richer interface to interact with the current state. This enables us to specify the
 * abstract semantics of many of TouchDevelops native functions in a more natural way.
 *
 * Lucas Brutschy
 * Date: 10/5/12
 * Time: 4:30 PM
 *
 */

object RichNativeSemantics extends RichExpressionImplicits {

  /*-- Checking / Reporting errors --*/

  def Dummy[S <: State[S]](obj: RichExpression, method: String)(implicit state: S, pp: ProgramPoint) {
    val currentClass = SystemParameters.analysisUnitContext.clazzType.toString
    if (TouchAnalysisParameters.reportDummyImplementations &&
      (!TouchAnalysisParameters.reportOnlyAlarmsInMainScript || currentClass.equals(SystemParameters.compiler.asInstanceOf[TouchCompiler].main.toString)))
      Reporter.reportDummy(obj.getType().toString + "->" + method, pp)
  }

  def Dummy[S <: State[S]](text: String)(implicit state: S, pp: ProgramPoint) {
    val currentClass = SystemParameters.analysisUnitContext.clazzType.toString
    if (TouchAnalysisParameters.reportDummyImplementations &&
      (!TouchAnalysisParameters.reportOnlyAlarmsInMainScript || currentClass.equals(SystemParameters.compiler.asInstanceOf[TouchCompiler].main.toString)))
      Reporter.reportDummy(text, pp)
  }

  def Error[S <: State[S]](expr: RichExpression, message: String)(implicit state: S, pp: ProgramPoint): S = {
    if (Reporter.enableOutputOfAlarms) {
      val errorState = state.assume(expr).setExpression(ExpressionSet(new UnitExpression(SystemParameters.typ.top(), pp)))
      if (!errorState.isBottom) {
        val currentClass = SystemParameters.analysisUnitContext.clazzType.toString
        if (!TouchAnalysisParameters.reportOnlyAlarmsInMainScript
          || currentClass.equals(SystemParameters.compiler.asInstanceOf[TouchCompiler].main.toString)) {
          Reporter.reportError(message, pp, state.explainError(expr))
        }
        val ret = state.assume(expr.not())
        if (ret.isBottom) return ret.bottom()
        ret
      } else state
    } else state
  }

  def Error[S <: State[S]](expr: RichExpression, method: String, message: String)(implicit state: S, pp: ProgramPoint): S = {
    Error[S](expr, "When calling " + method + ": " + message)
  }

  def CheckInRangeInclusive[S <: State[S]](expr: RichExpression, low: RichExpression, high: RichExpression, method: String, parameter: String)(implicit s: S, pp: ProgramPoint): S = {
    if (TouchAnalysisParameters.printValuesInWarnings) {
      val state1 = Error(expr < low, method + ": Parameter " + parameter + " (" + expr + ") may be less than the lowest allowed value (" + low + ")")(s, pp)
      Error(expr > high, method + ": Parameter " + parameter + " (" + expr + ") may be greater than the highest allowed value " + high + ")")(state1, pp)
    } else {
      val state1 = Error(expr < low, method + ": Parameter " + parameter + " may be less than the lowest allowed value")(s, pp)
      Error(expr > high, method + ": Parameter " + parameter + " may be greater than the highest allowed value")(state1, pp)
    }
  }

  def CheckNonNegative[S <: State[S]](expr: RichExpression, method: String, parameter: String)(implicit s: S, pp: ProgramPoint): S = {
    if (TouchAnalysisParameters.printValuesInWarnings)
      Error(expr < 0, method + ": Parameter " + parameter + " (" + expr + ") may be negative")(s, pp)
    else
      Error(expr < 0, method + ": Parameter " + parameter + " may be negative")(s, pp)
  }

  def If[S <: State[S]](expr: RichExpression, Then: S => S, Else: S => S)(implicit state: S, pp: ProgramPoint): S = {
    val thenState = state.assume(expr)
    val elseState = state.assume(expr.not())

    if (thenState.isBottom) {
      Else(elseState)
    } else if (elseState.isBottom) {
      Then(thenState)
    } else {
      val thenRes = Then(thenState)
      val elseRes = Else(elseState)
      val res = thenRes.lub(elseRes)
      res
    }
  }

  def Default[S <: State[S]](typ: TouchType)(implicit s: S, pp: ProgramPoint): S = {
    s.setExpression(typ.name match {
      case "String" => ExpressionSet(Constant("", typ, pp))
      case "Number" => ExpressionSet(Constant("0", typ, pp))
      case "Boolean" => ExpressionSet(Constant("false", typ, pp))
      case _ => ExpressionSet(InvalidExpression(typ, "May be uninitialized", pp))
    })
  }

  /**
   * Creates a new Object of type typ, and initializes its fields with the given arguments.
   */
  def New[S <: State[S]](typ: TouchType,
                         initials: Map[ApiField, RichExpression] = Map.empty[ApiField, RichExpression],
                         initializeFields: Boolean = true)(implicit s: S, pp: ProgramPoint): S = {

    typ match {
      case TNumber => s.setExpression(ExpressionSet(Constant("0", TNumber, pp)))
      case TBoolean => s.setExpression(new ExpressionSet(TBoolean).add(False))
      case TString => s.setExpression(ExpressionSet(Constant("", TString, pp)))
      case anyTyp:AAny =>
        var curState = s.createObject(typ, pp)
        val obj = curState.expr

        if (initializeFields) {
          // Assign fields with given arguments
          for (f <- anyTyp.representedTouchFields) {
            val (newPP, referenceLoop) = DeepeningProgramPoint(pp, f.getName)
            val a = initials.get(f) match {
              case None => f.default match {
                case InvalidInitializer(r) =>
                  Invalid(f.typ, r)
                case TopInitializer =>
                  curState = Top[S](f.typ, initializeFields = !referenceLoop)(curState, newPP)
                  toRichExpression(curState.expr)
                case TopWithInvalidInitializer(r) =>
                  curState = TopWithInvalid[S](f.typ, r, initializeFields = !referenceLoop)(curState, newPP)
                  toRichExpression(curState.expr)
                case NewInitializer =>
                  curState = New[S](f.typ, initializeFields = !referenceLoop)(curState, newPP)
                  toRichExpression(curState.expr)
                case ExpressionInitializer(e) => e
              }
              case Some(st) => st
            }
            curState = AssignField[S](obj, f, a)(curState, pp)
          }
        }

        curState.setExpression(obj)
    }
  }

  def Top[S <: State[S]](typ: TouchType,
                         initials: Map[Identifier, RichExpression] = Map.empty[Identifier, RichExpression],
                         initializeFields: Boolean = true)
                        (implicit s: S, pp: ProgramPoint): S = {
    typ match {
      case TNumber => s.setExpression(Valid(TNumber))
      case TBoolean => s.setExpression(new ExpressionSet(TBoolean).add(True).add(False))
      case TString => s.setExpression(Valid(TString))
      case anyType:AAny =>

        var curState = s.createObject(typ, pp)
        val obj = curState.expr

        // TODO: Support for recursive datastructures
//        val tempVariable = VariableIdentifier("initVar"+pp.toString)(typ,pp)
//        curState = s.createVariable(toRichExpression(tempVariable),typ,pp)
//        curState = s.assignVariable(toRichExpression(tempVariable),obj)

        if (initializeFields) {
          // Assign fields with given arguments
          for (f <- anyType.representedTouchFields) {
            val (newPP, referenceLoop) = DeepeningProgramPoint(pp, f.getName)
            val a = initials.get(f) match {
              case None => f.topDefault match {
                case InvalidInitializer(r) => Invalid(f.typ, r)
                case TopInitializer => curState = Top[S](f.typ, initializeFields = !referenceLoop)(curState, newPP); toRichExpression(curState.expr)
                case TopWithInvalidInitializer(r) => curState = TopWithInvalid[S](f.typ, r, initializeFields = !referenceLoop)(curState, newPP); toRichExpression(curState.expr)
                case NewInitializer => curState = New[S](f.typ, initializeFields = !referenceLoop)(curState, newPP); toRichExpression(curState.expr)
                case ExpressionInitializer(e) => e
              }
              case Some(st) => st
            }
            curState = AssignField[S](obj, f, a)(curState, pp)
          }
        }

        curState.setExpression(obj)

    }
  }

  def TopWithInvalid[S <: State[S]](typ: TouchType,
                                    invalidCause: String,
                                    initials: Map[Identifier, RichExpression] = Map.empty[Identifier, RichExpression],
                                    initializeFields: Boolean = true)(implicit s: S, pp: ProgramPoint): S = {

    val curState = Top[S](typ, initials, initializeFields = initializeFields)(s, pp)
    val validResult = curState.expr
    Return[S](validResult, Invalid(typ, invalidCause))(curState, pp)

  }

  def SetToTopWithInvalid[S <: State[S]](expr: RichExpression, invalidCause: String)(implicit s: S, pp: ProgramPoint): S = {
    Assign[S](expr,Valid(expr.getType()) or Invalid(expr.getType(),invalidCause)) // FIXME: Unsound: Handle object types
  }

  def SetToTop[S <: State[S]](expr: RichExpression)(implicit s: S, pp: ProgramPoint): S = {
    Assign[S](expr,Valid(expr.getType())) // FIXME: Unsound: Handle object types
  }

  def Clone[S <: State[S]](obj: RichExpression, initials: Map[Identifier, RichExpression] = Map.empty[Identifier, RichExpression], recursive: Boolean = true)(implicit s: S, pp: ProgramPoint): S = {

    val touchTyp = obj.getType().asInstanceOf[AAny]

    // Never clone immutable types where we don't change the fields. This includes all primitives
    if (touchTyp.isImmutable && initials.isEmpty) {
      return Return[S](obj)
    }

    var curState = New[S](touchTyp, initializeFields = false)(s, pp)
    val newObject = toRichExpression(curState.expr)

    // Clone fields
    for (f <- obj.getType().asInstanceOf[AAny].representedTouchFields) {
      initials.get(f) match {
        case None =>
          if (recursive) {
            val (newPP, referenceLoop) = DeepeningProgramPoint(pp, f.getName)
            val oldField = Field[S](obj, f)(curState, newPP)
            curState = Clone[S](oldField, recursive = !referenceLoop)(curState, newPP)
            val clonedContent = curState.expr
            curState = AssignField[S](newObject, f, clonedContent)(curState, newPP)
          } else {
            val oldField = Field[S](obj, f)(curState, pp)
            curState = AssignField[S](newObject, f, oldField)(curState, pp)
          }
        case Some(st) =>
          curState = AssignField[S](newObject, f, st)(curState, pp)
      }
    }

    curState.setExpression(newObject)

  }

  /*-- Misc --*/

  def Assume[S <: State[S]](expr: RichExpression)(implicit state: S, pp: ProgramPoint): S = {
    state.assume(expr)
  }

  def CallLocalAction[S <: State[S]](method: String, parameters: List[ExpressionSet] = Nil)
                                    (implicit state: S, pp: ProgramPoint): S = {
    val context = SystemParameters.analysisUnitContext
    val classType = context.clazzType
    SystemParameters.compiler.asInstanceOf[TouchCompiler].getMethodWithClassDefinition(method, classType, parameters map (_.getType())) match {
      case Some(mdecl) =>
        val res = MethodSummaries.collect(pp, mdecl, state, parameters)
        res
      case _ =>
        Reporter.reportImprecision("Could not find method " + method, pp)
        state.top()
    }
  }

  def CallApi[S <: State[S]](obj: RichExpression, method: String, parameters: List[ExpressionSet] = Nil, returnedType: TouchType)(implicit state: S, pp: ProgramPoint): S = {
    obj.getType().asInstanceOf[AAny].forwardSemantics(obj, method, parameters, returnedType)(pp, state)
  }

  def Return[S <: State[S]](e: RichExpression*)(implicit state: S, pp: ProgramPoint): S = {
    lazy val typ = e.head.getType()
    var set = new ExpressionSet(typ)
    for (ex <- e) {
      set = set.add(ex.thisExpr)
    }
    val stateN = state.setExpression(set)
    stateN
  }

  def Assign[S <: State[S]](id: RichExpression, value: RichExpression)(implicit state: S, pp: ProgramPoint): S = {

    def join(a: List[ExpressionSet], b: List[ExpressionSet]): List[ExpressionSet] = {
      a match {
        case x :: xs =>
          b match {
            case y :: ys => x.add(y) :: join(xs, ys)
            case Nil => a
          }
        case Nil => b
      }
    }

    def getMultiValAsList(expr: RichExpression): List[ExpressionSet] = {
      var ret: List[ExpressionSet] = Nil
      for (sExpr <- expr.thisExpr.getSetOfExpressions) yield {
        sExpr match {
          case MultiValExpression(left, right, retVal) =>
            val multiVal = getMultiValAsList(left) ::: getMultiValAsList(right)
            ret = join(ret, multiVal)
          case _ =>
            ret = join(ret, List(new ExpressionSet(expr.getType()).add(expr)))
        }
      }
      ret
    }

    val leftExprs = getMultiValAsList(id)
    val rightExprs = getMultiValAsList(value)

    if (leftExprs.length != rightExprs.length) {
      Reporter.reportImprecision("An assignment has an unmatching number of values - going to top", pp)
      return state.top()
    }

    var curState = state
    for ((l, r) <- leftExprs.zip(rightExprs)) {

      // Create variable that might not exist yet
      if (l.getSetOfExpressions.size == 1 && l.getSetOfExpressions.head.isInstanceOf[VariableIdentifier]) {
        curState = curState.createVariable(l, l.getType(), pp)
      }

      curState = curState.assignVariable(l, r)
    }
    curState

  }

  /*-- Reading and writing of fields --*/

  def AssignField[S <: State[S]](obj: RichExpression, field: Identifier, value: RichExpression)(implicit state: S, pp: ProgramPoint): S = {
    AssignField[S](obj, field.getName, value)
  }

  def AssignField[S <: State[S]](obj: RichExpression, field: String, value: RichExpression)(implicit state: S, pp: ProgramPoint): S = {
    if (obj.getType().representedFields.exists(x => x.getField match { case None => false; case Some(y) => y == field})) {
      state.assignField(obj, field, value)
    } else state
  }

  def Field[S <: State[S]](obj: RichExpression, field: ApiField)(implicit state: S, pp: ProgramPoint): RichExpression = {
    obj.thisExpr.getType().asInstanceOf[AAny].forwardSemantics(obj,field.getName,Nil,field.typ).expr
  }

  /*-- Skipping --*/

  def Exit[S <: State[S]](implicit state: S, pp: ProgramPoint): S = MethodSummaries.collectExit[S](state)

  def Skip[S <: State[S]](implicit state: S, pp: ProgramPoint): S = state.removeExpression()

  def Unimplemented[S <: State[S]](method: String)(implicit state: S, pp: ProgramPoint): S = {
    Reporter.reportImprecision(method + " not implemented, going to top", pp)
    state.top()
  }

}

/**
 * @param isAccumulating a field that refers to zero to many objects. These play a key role in the analysis and are
 *                       used to model collections.
 */
case class ApiField(name: String,
                  typ: AAny,
                  default: Initializer = NewInitializer,
                  topDefault: Initializer = TopInitializer,
                  isSummaryNode: Boolean = false,
                  isAccumulating: Boolean = false)
  extends Identifier {

  val pp = null

  override def getName = name.toString

  override def toString = name.toString

  override def getField = Some(name)

  override def hashCode(): Int = name.hashCode() + typ.typeName.hashCode()

  override def representsSingleVariable = !isSummaryNode
}

trait Initializer

case class InvalidInitializer(invalidReason: String) extends Initializer

case object NewInitializer extends Initializer

case object TopInitializer extends Initializer

case class TopWithInvalidInitializer(invalidReason: String) extends Initializer

case class ExpressionInitializer(e: RichExpression) extends Initializer
