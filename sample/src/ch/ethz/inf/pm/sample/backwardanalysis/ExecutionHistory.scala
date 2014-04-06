package ch.ethz.inf.pm.sample.backwardanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}

/**
 * Stores history of abstract operations ("transfer functions") that were performed
 * on an abstract  state during the forward analysis, along with intermediate states.
 *
 * The history can be linear or branch with conditionals.
 */
abstract class ExecutionHistory[S <: State[S]]

case class NilNode[S <: State[S]](pre: S) extends ExecutionHistory[S] {
  override def toString: String = "NilNode"
}

case class OpNode[S <: State[S]](pre: S, op: SemanticOp[S], predecessor: ExecutionHistory[S]) extends ExecutionHistory[S] {
  override def toString: String = predecessor.toString + "\n" + op.toString
}

case class CondNode[
S <: State[S]](
                pre: S,
                cond: ExpressionSet,
                trueHistory: ExecutionHistory[S],
                afterTrueBranchState: S,
                falseHistory: ExecutionHistory[S],
                afterFalseBrachState: S,
                predecessor: ExecutionHistory[S])
  extends ExecutionHistory[S] {

  override def toString: String = {
    def indent(str: String, spaces: Int) = {
      val indentation = " " * spaces
      val indentedLines = str.lines map (indentation + _)
      indentedLines.mkString("\n") + "\n"
    }

    predecessor.toString + "\n" + s"CondNode cond: $cond {\n" +
      indent(trueHistory.toString, 2) + "} {" +
      indent(falseHistory.toString, 2) + "}\n"
  }
}