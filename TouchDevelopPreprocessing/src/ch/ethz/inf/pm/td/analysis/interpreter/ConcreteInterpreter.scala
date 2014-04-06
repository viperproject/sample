package ch.ethz.inf.pm.td.analysis.interpreter

import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.td.semantics.{SData, AAny}
import ch.ethz.inf.pm.td.analysis.TouchAnalysisParameters
import ch.ethz.inf.pm.sample.oorepresentation.VariableDeclaration
import scala.Some
import ch.ethz.inf.pm.sample.oorepresentation.Variable
import ch.ethz.inf.pm.sample.oorepresentation.ConstantStatement
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.oorepresentation.MethodCall
import ch.ethz.inf.pm.td.analysis.backward.InterpreterTestInput
import ch.ethz.inf.pm.sample.reporting.SampleError
import ch.ethz.inf.pm.sample.oorepresentation.EmptyStatement
import ch.ethz.inf.pm.sample.oorepresentation.FieldAccess
import ch.ethz.inf.pm.sample.SystemParameters

case class ActualMethodParams(values: Map[VariableIdentifier, TouchValue] = Map.empty)

object ConcreteInterpreter extends InterpreterImplicits

class ConcreteInterpreter(val compiler: TouchCompiler, val input: InterpreterTestInput) {
  val state = input.initialState

  val maxiter = 1000
  var iter = 0

  def run(): InterpreterResult = {
    try {
      executeMethod(input.startMethod, input.startMethodActualParams)
      SuccessfulExecution
    } catch {
      case ie: InterpreterException =>
        SystemParameters.progressOutput.put(ie.toString)
        FailedExecution(ie.error)
    }
  }


  def executeMethod(method: MethodDeclaration, params: ActualMethodParams, callPoint: Option[ProgramPoint] = None): TouchValue = {
    enterMethod(method, params, callPoint)
    val startBlockId = 0
    iterateBlocks(startBlockId)
    exitMethod(method, params)
  }

  def resolveAndExecuteMethod(method: String, params: List[TouchValue], callPoint: Option[ProgramPoint] = None): TouchValue = {
    val classType = state.currentMethod.ownerType
    compiler.getMethodWithClassDefinition(method, classType, params.map(_.typ)) match {
      case Some(mdecl) =>
        val varIds = mdecl.arguments(0).map(_.variable).map(_.id)
        val paramMap = varIds.zip(params).toMap
        // A sound type checker should not allow the following to be violated
        paramMap.foreach({ case (id, touchVal) => assert(id.typ.name == touchVal.typ.name) })
        executeMethod(mdecl, ActualMethodParams(paramMap), callPoint)
      case None =>
        failInternal("Given method not found")
    }
  }

  def enterMethod(method: MethodDeclaration, params: ActualMethodParams, callPoint: Option[ProgramPoint] = None) = {
    state.methodStack ::= method
    val cp = callPoint.getOrElse(method.programpoint)
    state.callPoints ::= cp
    state.envStack ::= collection.mutable.Map.empty

    val paramInitValues = params.values

    for (VariableDeclaration(_, Variable(_, varId), typ, _) <- method.arguments(0)) {
      val touchVal = typ.name match {
        case "Number" =>
          paramInitValues.getOrElse(varId, NumberV(0))
        case "Boolean" =>
          paramInitValues.getOrElse(varId, BooleanV(v = false))
        case "String" =>
          paramInitValues.getOrElse(varId, StringV(""))
        case _ =>
          paramInitValues.getOrElse(varId, InvalidV(typ.asInstanceOf[TouchType]))
      }

      state.setVar(varId, touchVal)
    }
  }

  def exitMethod(mdecl: MethodDeclaration, params: ActualMethodParams): TouchValue = {
    val returnVarIds = mdecl.arguments(1).map(_.variable).map(_.id)
    val returnVal =
      if (returnVarIds.length == 0) UnitV
      else if (returnVarIds.length == 1) state.getVar(returnVarIds.head)
      else ???

    state.methodStack = state.methodStack.tail
    state.callPoints = state.callPoints.tail
    state.envStack = state.envStack.tail
    returnVal
  }

  def assertE(e: => Boolean, errorType: InterpreterErrorType.Value = InterpreterErrorType.AssertFailure, detailedMessage: Option[String] = None)(implicit pp: ProgramPoint) = {
    if (!e) {
      failWithError(pp, errorType, detailedMessage)
    }
  }

  def failWithError(pp: ProgramPoint, errorType: InterpreterErrorType.Value, detailedMessage: Option[String] = None) = {
    val message = InterpreterErrorType.explanationOf(errorType) + (detailedMessage map (": " + _)).getOrElse("")
    val errorId = InterpreterErrorType.idOf(errorType)
    val error = SampleError(errorId, message, pp)
    //Reporter.reportError(error)
    throw new InterpreterException(error)
  }

  def failInternal(message: String): Nothing = {
    sys.error(message)
  }

  def iterateBlocks(blockId: Int): TouchValue = {
    iter = iter + 1
    if (iter > maxiter) {
      val pp = state.currentMethod.programpoint
      failWithError(pp, InterpreterErrorType.NonTermination)
    }
    val result = executeBlock(blockId)
    result.successor match {
      case Some(succId) =>  iterateBlocks(succId)
      case None => result.returnValue
    }
  }

  case class BlockResult(returnValue: TouchValue, successor: Option[Int])

  def executeBlock(blockId: Int): BlockResult = {
    val cfg = state.currentCFG
    val blockStmts = cfg.getBasicBlockStatements(blockId)
    val retVals = for (stmt <- blockStmts) yield  executeStatement(stmt)
    val lastVal = retVals.lastOption // blocks may be empty
    val succBlockIds = cfg.getDirectSuccessors(blockId)
    val successor = if (succBlockIds.size == 0) None // exit
    else if (succBlockIds.size == 1) Some(succBlockIds.head)
    else {
      assert (succBlockIds.size == 2)
      val Some((_, trueBranch, falseBranch)) = cfg.getExitConditions(blockId)
      val conditionVal = lastVal match {
        case Some(BooleanV(b)) => b
        case _ => failInternal("Condition did not evaluate to boolean value!")
      }
      if (conditionVal) Some(trueBranch) else Some(falseBranch)
    }
    BlockResult(lastVal.getOrElse(UnitV), successor)
  }

  def executeAssigment(state: ConcreteInterpreterState, lhsStmt: Statement, rhsStmts: List[Statement]): TouchValue = {
    assert(rhsStmts.size == 1)
    val rhs = rhsStmts.head
    val rhsVal = executeStatement(rhs)
    lhsStmt match {
      // simple case: x := expr
      case Variable(_, vid) => state.setVar(vid, rhsVal)
      // field assignment: x->f := expr
      case m@MethodCall(pp, FieldAccess(_, objStmt, methodName, _), _, _, returnType) =>
        val targetRef = checkRef(pp, executeStatement(objStmt))
        checkValid(pp, targetRef)
        if (targetRef.typ == SData.typ) {
          // exception: data->f := expr is actually a global variable assignment
          val varId = compiler.resolveGlobalData(methodName)
          state.setGlobal(varId, rhsVal)
        } else {
          // normal field assignment
          state.setField(targetRef, methodName, rhsVal)
        }
    }
    UnitV
  }

  def checkRef(pp: ProgramPoint, expr: TouchValue): RefV = {
    expr match {
      case ref: RefV => ref
      case _ => failWithError(pp, InterpreterErrorType.WrongType)
    }
  }

  def checkValid(pp: ProgramPoint, expr: TouchValue, message: Option[String] = None): TouchValue = {
    expr match {
      case InvalidV(typ) => failWithError(pp, InterpreterErrorType.InvalidTarget, message)
      case _ => expr
    }
  }

  def checkValidTarget(method: String, pp: ProgramPoint, expr: TouchValue) = {
    if (method != "is invalid")  checkValid(pp, expr, Some(s"(method $method"))
  }

  def checkValidArgs(method: String, pp: ProgramPoint, paramExprs: List[TouchValue]) = {
    for (paramE <- paramExprs) {
      paramE match {
        case InvalidV(typ) => failWithError(pp, InterpreterErrorType.InvalidArgument)
        case _ =>
      }
    }
  }

  def stackDependentPP(pp: ProgramPoint): ProgramPoint = {
    if (TouchAnalysisParameters.contextSensitiveInterproceduralAnalysis) {
      val entryPPs = state.callPoints.map(_.toString).toSet.toList.sorted
      DeepeningProgramPoint(pp, entryPPs)
    } else pp
  }

  def nonDetInputAt(pp: ProgramPoint): Option[TouchValue] = {
    input.nonDetInputAt(stackDependentPP(pp))
  }

  def executeStatement(stmt: Statement): TouchValue = {
    stmt match {
      case ConstantStatement(_, stringValue, typ) => typ.name match {
        case "Number" => NumberV(stringValue.toDouble)
        case "String" => StringV(stringValue)
        case "Boolean" => BooleanV(stringValue.toBoolean)
      }
      case EmptyStatement(pp) => UnitV
      case Variable(_, vid) => state.getVar(vid)
      case m@MethodCall(pp, FieldAccess(_, objStmt, methodName, _), _, parameterStmts, returnType) =>
        if (methodName == ":=") {
          executeAssigment(state, objStmt, parameterStmts)
        } else {
          val targetExpr = executeStatement(objStmt)

          checkValidTarget(methodName, pp, targetExpr)

          val paramExprs = for (paramS <- parameterStmts) yield {
            executeStatement(paramS)
          }

          if (targetExpr.typ.name != "code") {
            checkValidArgs(methodName, pp, paramExprs)
          }

          val typeSemantics: AAny = methodSemantics(targetExpr)
          typeSemantics.concreteSemantics(targetExpr, methodName, paramExprs, this, pp)
        }
    }
  }

  def methodSemantics(touchVal: TouchValue): AAny = {
    val typ = touchVal.typ
    TypeList.types.get(typ.name) match {
      case Some(s) => s
      case None => sys.error("No method semantics found for type " + typ.name)
    }
  }
}