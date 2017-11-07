/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.analysis

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{Constant, UnitExpression, VariableIdentifier, _}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.sample.reporting.Reporter
import ch.ethz.inf.pm.sample.reporting.Reporter.{MessageClass, ReportingLevel}
import ch.ethz.inf.pm.td.compiler.{DeepeningProgramPoint, _}
import ch.ethz.inf.pm.td.domain.{InvalidExpression, MultiValExpression, TouchStateInterface}
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

object RichNativeSemantics extends RichExpressionSetImplicits {

  def Dummy[S <: State[S]](obj: RichExpressionSet, method: String)(implicit state: S, pp: ProgramPoint) {
    if (TouchAnalysisParameters.get.reportDummyImplementations)
      Reporter.reportDummyImplementation(obj.typ.toString + "->" + method, pp)
  }

  /*-- Checking / Reporting errors --*/

  def Error[S <: State[S]](expr: RichExpressionSet, method: String, message: String)(implicit state: S, pp: ProgramPoint): S = {
    Error[S](expr, "When calling " + method + ": " + message)
  }

  def Error[S <: State[S]](expr: RichExpressionSet, message: String)(implicit state: S, pp: ProgramPoint): S = {
    val errorState = state.assume(expr).setExpression(ExpressionSet(UnitExpression(SystemParameters.tm.Top, pp)))
    if (!errorState.isBottom && Reporter.reportingLevels(MessageClass.AssertionViolation) != ReportingLevel.Off) {
      if (isInReportableSection) {
        Reporter.reportAssertionViolation(message, pp, state.explainError(expr))
      } else if (TouchAnalysisParameters.get.libraryErrorReportingMode == LibraryErrorReportingMode.ReportAtBoundary) {
        Reporter.reportAssertionViolation("Something may go wrong inside a library call: "+message, SystemParameters.libraryBoundaryContext, state.explainError(expr))
      }
      val ret = state.assume(expr.not())
      if (ret.isBottom) return ret.bottom()
      ret
    } else state.assume(expr.not())
  }

  def isInReportableSection: Boolean = {
    if (TouchAnalysisParameters.get.libraryErrorReportingMode == LibraryErrorReportingMode.Report) return true
    val currentClass = SystemParameters.analysisUnitContext.clazz
    val mainClass = SystemParameters.compiler.asInstanceOf[TouchCompiler].main
    currentClass.equals(mainClass)
  }

  def CheckInRangeInclusive[S <: State[S]](expr: RichExpressionSet, low: RichExpressionSet, high: RichExpressionSet, method: String, parameter: String)(implicit s: S, pp: ProgramPoint): S = {
    if (TouchAnalysisParameters.get.printValuesInWarnings) {
      val state1 = Error(expr < low, method + ": Parameter " + parameter + " (" + expr + ") may be less than the lowest allowed value (" + low + ")")(s, pp)
      Error(expr > high, method + ": Parameter " + parameter + " (" + expr + ") may be greater than the highest allowed value " + high + ")")(state1, pp)
    } else {
      val state1 = Error(expr < low, method + ": Parameter " + parameter + " may be less than the lowest allowed value")(s, pp)
      Error(expr > high, method + ": Parameter " + parameter + " may be greater than the highest allowed value")(state1, pp)
    }
  }

  def CheckNonNegative[S <: State[S]](expr: RichExpressionSet, method: String, parameter: String)(implicit s: S, pp: ProgramPoint): S = {
    if (TouchAnalysisParameters.get.printValuesInWarnings)
      Error(expr < 0, method + ": Parameter " + parameter + " (" + expr + ") may be negative")(s, pp)
    else
      Error(expr < 0, method + ": Parameter " + parameter + " may be negative")(s, pp)
  }

  def If[S <: State[S]](expr: RichExpressionSet, Then: S => S, Else: S => S)(implicit state: S, pp: ProgramPoint): S = {
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
      case "String" => ExpressionSet(Constant("", typ)(pp))
      case "Number" => ExpressionSet(Constant("0", typ)(pp))
      case "Boolean" => ExpressionSet(Constant("false", typ)(pp))
      case _ => ExpressionSet(InvalidExpression(typ, "May be uninitialized", pp))
    })
  }

  /**
   * Creates a new Object of type typ, and initializes its fields with the given arguments.
   */
  def New[S <: State[S]](typ: TouchType,
      initials: Map[ApiField, RichExpressionSet] = Map.empty[ApiField, RichExpressionSet],
                         initializeFields: Boolean = true)(implicit s: S, pp: ProgramPoint): S = {

    typ match {
      case TNumber => s.setExpression(ExpressionSet(Constant("0", TNumber)(pp)))
      case TBoolean => s.setExpression(new ExpressionSet(TBoolean).add(False))
      case TString => s.setExpression(ExpressionSet(Constant("", TString)(pp)))
      case anyTyp:AAny =>

        var curState = s
        val fields = anyTyp.representedTouchFields.toList

        val tempVariables =
          if (initializeFields) {

            // Assign fields with given arguments
            for (f <- fields) yield {

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
                  case DefaultInitializer(why) =>
                    Default(f.typ,why)
                  case ExpressionInitializer(e) => e
                }
                case Some(st) => st
              }

              val tempVar:VariableIdentifier = VariableIdentifier("__TMP"+pp.toString+f.getName)(f.typ,pp)
              curState = curState.createVariable(ExpressionSet(tempVar),f.typ,pp)
              curState = curState.assignVariable(ExpressionSet(tempVar),toExpressionSet(a))
              tempVar
            }
          } else Nil

        curState = curState.createObject(typ, pp)
        val obj = curState.expr

        for ((f,v) <- fields.zip(tempVariables)) {
          curState = AssignField[S](obj, f, v)(curState, pp)
        }

        val tempVariablesSet = tempVariables.toSet
        curState = curState.pruneVariables(tempVariablesSet.contains)
        curState.setExpression(obj)
    }
  }

  def Top[S <: State[S]](typ: TouchType,
      initials: Map[Identifier, RichExpressionSet] = Map.empty[Identifier, RichExpressionSet],
                         initializeFields: Boolean = true)
                        (implicit s: S, pp: ProgramPoint): S = {
    typ match {
      case TNumber => s.setExpression(Valid(TNumber))
      case TBoolean => s.setExpression(new ExpressionSet(TBoolean).add(True).add(False))
      case TString => s.setExpression(Valid(TString))
      case anyType:AAny =>

        var curState = s
        val fields = anyType.representedTouchFields.toList

        val tempVariables =
          if (initializeFields) {
            // Assign fields with given arguments
            for (f <- fields) yield {

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

              val tempVar:VariableIdentifier = VariableIdentifier("__TMP"+pp.toString+f.getName)(f.typ,pp)
              curState = curState.createVariable(ExpressionSet(tempVar),f.typ,pp)
              curState = curState.assignVariable(ExpressionSet(tempVar),toExpressionSet(a))
              tempVar
            }
          } else Nil

        curState = curState.createObject(typ, pp)
        val obj = curState.expr

        for ((f,v) <- fields.zip(tempVariables)) {
          curState = AssignField[S](obj, f, v)(curState, pp)
        }

        val tempVariablesSet = tempVariables.toSet
        curState = curState.pruneVariables(tempVariablesSet.contains)
        curState.setExpression(obj)

    }
  }

  def TopWithInvalid[S <: State[S]](typ: TouchType,
                                    invalidCause: String,
      initials: Map[Identifier, RichExpressionSet] = Map.empty[Identifier, RichExpressionSet],
                                    initializeFields: Boolean = true)(implicit s: S, pp: ProgramPoint): S = {

    val curState = Top[S](typ, initials, initializeFields = initializeFields)(s, pp)
    val validResult = curState.expr
    Return[S](validResult, Invalid(typ, invalidCause))(curState, pp)

  }

  def SetToTopWithInvalid[S <: State[S]](expr: RichExpressionSet, invalidCause: String)(implicit s: S, pp: ProgramPoint): S = {
    Assign[S](expr,Valid(expr.typ) or Invalid(expr.typ,invalidCause)) // FIXME: Unsound: Handle object types
  }

  def SetToTop[S <: State[S]](expr: RichExpressionSet)(implicit s: S, pp: ProgramPoint): S = {
    Assign[S](expr,Valid(expr.typ)) // FIXME: Unsound: Handle object types
  }

  def Clone[S <: State[S]](obj: RichExpressionSet, initials: Map[Identifier, RichExpressionSet] = Map.empty[Identifier, RichExpressionSet], recursive: Boolean = true)(implicit s: S, pp: ProgramPoint): S = {

    var curState = s
    val anyType = obj.typ.asInstanceOf[AAny]

    // Never clone immutable types where we don't change the fields. This includes all primitives
    if (anyType.isImmutable && initials.isEmpty) {
      return Return[S](obj)
    }

    val fieldList = anyType.representedTouchFields.toList

    // Clone fields
    val tempVariables =
      for (f <- fieldList) yield {
        val a =
          initials.get(f) match {
            case None =>
              if (recursive) {
                val (newPP, referenceLoop) = DeepeningProgramPoint(pp, f.getName)
                val oldField = Field[S](obj, f)(curState, newPP)
                curState = Clone[S](oldField, recursive = !referenceLoop)(curState, newPP)
                toRichExpression(curState.expr)
              } else Field[S](obj, f)(curState, pp)
            case Some(st) => st
          }

        val tempVar:VariableIdentifier = VariableIdentifier("__TMP"+pp.toString+f.getName)(f.typ,pp)
        curState = curState.createVariable(ExpressionSet(tempVar),f.typ,pp)
        curState = curState.assignVariable(ExpressionSet(tempVar),toExpressionSet(a))
        tempVar
      }

    curState = curState.createObject(anyType, pp)
    val newObj = curState.expr

    for ((f,v) <- fieldList.zip(tempVariables)) {
      curState = AssignField[S](newObj, f, v)(curState, pp)
    }

    val tempVariablesSet = tempVariables.toSet
    curState = curState.pruneVariables(tempVariablesSet.contains)
    curState.setExpression(newObj)
  }

  /*-- Misc --*/

  def Assume[S <: State[S]](expr: RichExpressionSet)(implicit state: S, pp: ProgramPoint): S = {
    state.assume(expr)
  }


  def CallLocalAction[S <: State[S]](method: String, parameters: List[ExpressionSet] = Nil)
                                    (implicit state: S, pp: ProgramPoint): S = {
    val context = SystemParameters.analysisUnitContext
    val classType = context.clazzType
    SystemParameters.compiler.asInstanceOf[TouchCompiler].getMethodWithClassDefinition(method, classType, parameters map (_.typ)) match {
      case Some(m) =>
        val res = MethodSummaries.collect(pp, m, state, parameters)
        res
      case _ =>
        Reporter.reportImpreciseSemantics("Could not find method " + method, pp)
        state.top()
    }
  }

  def CallApi[S <: State[S]](obj: RichExpressionSet, method: String, parameters: List[ExpressionSet] = Nil, returnedType: TouchType)(implicit state: S, pp: ProgramPoint): S = {
    if (obj.isBottom) state.bottom()
    else obj.typ.asInstanceOf[AAny].forwardSemantics(obj, method, parameters, returnedType)(pp, state)
  }

  def Return[S <: State[S]](e: RichExpressionSet*)(implicit state: S, pp: ProgramPoint): S = {
    if (e.size > 1) {
      lazy val typ = e.head.typ
      var set = new ExpressionSet(typ)
      for (ex <- e) {
        set = set.add(ex.thisExpr)
      }
      val stateN = state.setExpression(set)
      stateN
    } else if (e.size == 1) {
      state.setExpression(e.head)
    } else {
      state.setExpression(ExpressionSet().bottom())
    }
  }

  def Assign[S <: State[S]](id: RichExpressionSet, value: RichExpressionSet)(implicit state: S, pp: ProgramPoint): S = {

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

    def getMultiValAsList(expr: RichExpressionSet): List[ExpressionSet] = {
      var ret: List[ExpressionSet] = Nil
      for (sExpr <- expr.thisExpr.toSetOrFail) yield {
        sExpr match {
          case MultiValExpression(left, right, retVal) =>
            val multiVal = getMultiValAsList(left) ::: getMultiValAsList(right)
            ret = join(ret, multiVal)
          case _ =>
            ret = join(ret, List(new ExpressionSet(expr.typ).add(expr)))
        }
      }
      ret
    }

    if (value.isBottom) {
      return state.bottom()
    }

    if (id.thisExpr.isTop || id.thisExpr.s.isTop) return state.top()

    val leftExprs = getMultiValAsList(id)
    val rightExprs = getMultiValAsList(value)

    if (leftExprs.length != rightExprs.length || value.thisExpr.isTop) {
      Reporter.reportImpreciseSemantics("An assignment has an unmatching number of values or right side is top - setting result to top", pp)
      var curState = state
      if (TouchAnalysisParameters.get.defaultToUnsound) {
        for (l <- leftExprs) {
          curState = TopWithInvalid[S](l.typ.asInstanceOf[TouchType],"Assignment failed")(curState,pp)
          curState = Assign[S](l,curState.expr)(curState,pp)
        }
      } else {
        for (l <- leftExprs) {
          curState = Top[S](l.typ.asInstanceOf[TouchType])(curState,pp)
          curState = Assign[S](l,curState.expr)(curState,pp)
        }
      }
      return curState
    }

    var curState = state
    for ((l, r) <- leftExprs.zip(rightExprs)) {

      // Create variable that might not exist yet
      if (l.toSetOrFail.size == 1 && l.toSetOrFail.head.isInstanceOf[VariableIdentifier]) {
        curState = curState.createVariable(l, l.typ, pp)
      }

      curState = curState.assignVariable(l, r)
    }
    curState

  }

  def EvalConstant[S <: State[S]](id: RichExpressionSet)(implicit state: S, pp: ProgramPoint): SetDomain.Default[Constant] = {
    id.s match {
      case SetDomain.Default.Bottom() => SetDomain.Default.Bottom()
      case SetDomain.Default.Top() => SetDomain.Default.Top()
      case SetDomain.Default.Inner(expr) =>
        Lattice.bigLub(expr map {
          case c:Constant => SetDomain.Default.Inner[Constant](Set(c))
          case i:Identifier => state.asInstanceOf[TouchStateInterface[_]].getPossibleConstants(i)
          case _ => SetDomain.Default.Top[Constant]()
        })
    }
  }

  /*-- Reading and writing of fields --*/

  def AssignField[S <: State[S]](obj: RichExpressionSet, field: Identifier, value: RichExpressionSet)(implicit state: S, pp: ProgramPoint): S = {
    AssignField[S](obj, field.getName, value)
  }

  def AssignField[S <: State[S]](obj: RichExpressionSet, field: String, value: RichExpressionSet)(implicit state: S, pp: ProgramPoint): S = {
    if (obj.typ.representedFields.exists(x => x.getField match { case None => false; case Some(y) => y == field})) {
      val res = state.assignField(obj, field, value)
      if (SystemParameters.DEBUG && !state.isBottom && res.isBottom)
        throw TouchException("Became bottom due to assignment")
      res
    } else state
  }

  def Field[S <: State[S]](obj: RichExpressionSet, field: ApiField)(implicit state: S, pp: ProgramPoint): RichExpressionSet = {
    if (obj.isBottom || state.isBottom) Bottom(field.typ)
    else obj.thisExpr.typ.asInstanceOf[AAny].forwardSemantics(obj,field.getName,Nil,field.typ).expr
  }

  /*-- Skipping --*/

  def Exit[S <: State[S]](implicit state: S, pp: ProgramPoint): S = MethodSummaries.collectExit[S](state)

  def Skip[S <: State[S]](implicit state: S, pp: ProgramPoint): S = state.removeExpression()

  def Unimplemented[S <: State[S]](method: String, returnedType:TouchType)(implicit state: S, pp: ProgramPoint): S = {
    if (!TouchAnalysisParameters.get.failOnMissingApi) {
      Reporter.reportImpreciseSemantics(method + " not implemented, going to top", pp)
      if (TouchAnalysisParameters.get.defaultToUnsound) {
        Top[S](returnedType)
      } else {
        state.top()
      }
    } else {
      throw TouchException(method + " not implemented")
    }
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

  override def getName = name

  override def toString = name

  override def getField = Some(name)

  override def hashCode(): Int = name.hashCode() + typ.typeName.hashCode()

  override def representsSingleVariable = !isSummaryNode
}

trait Initializer

case class InvalidInitializer(invalidReason: String) extends Initializer

/** Default initializer for variables. Invalid for object types, false, 0.0, "" for primitives */
case class DefaultInitializer(invalidReason: String) extends Initializer

case object NewInitializer extends Initializer

case object TopInitializer extends Initializer

case class TopWithInvalidInitializer(invalidReason: String) extends Initializer

case class ExpressionInitializer(e: RichExpressionSet) extends Initializer
