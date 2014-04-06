package ch.ethz.inf.pm.td.analysis.interpreter

import ch.ethz.inf.pm.sample.reporting.SampleError


object InterpreterErrorType extends Enumeration {
  type InterpreterErrorType = Value

  val NonTermination = Value
  val AssertFailure = Value
  val InvalidTarget = Value
  val InvalidArgument = Value
  val DivByZero = Value
  val WrongType = Value



  def idOf(error: InterpreterErrorType): String = error match {
    case NonTermination => "interpreter:nontermination"
    case AssertFailure => "assert.failed"
    case InvalidTarget => "methodtarget.invalid"
    case InvalidArgument => "methodparameter:invalid"
    case DivByZero => "assert.failed"
    case WrongType => "wrong.type"

  }

  def explanationOf(error: InterpreterErrorType): String = error match {
    case NonTermination => "Possibly non-terminating execution"
    case AssertFailure => "Failed assertion"
    case InvalidTarget => "Method call on invalid target"
    case InvalidArgument => "Method call with invalid argument"
    case DivByZero => "Division by zero"
    case WrongType => "Different type expected"




  }
}

case class InterpreterException(error: SampleError) extends Exception {
  override def toString = s"Interpreter terminated (error id ${error.id}, pp ${error.pp}): ${error.message}"
}

sealed trait InterpreterResult
case object SuccessfulExecution extends InterpreterResult
case class FailedExecution(error: SampleError) extends InterpreterResult