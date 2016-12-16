/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.cloud

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{State, _}
import ch.ethz.inf.pm.sample.execution.NodeWithState
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, ProgramPoint}
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._
import ch.ethz.inf.pm.td.analysis.TouchAnalysisParameters
import ch.ethz.inf.pm.td.compiler.{CloudEnabledModifier, TouchCompiler}
import ch.ethz.inf.pm.td.domain.{FieldIdentifier, TouchStateInterface}
import ch.ethz.inf.pm.td.semantics._

/**
  * @author Lucas Brutschy
  */
object CloudAnalysisState {

  private var programOrder: Map[(AbstractEvent, AbstractEvent), _] = Map.empty
  private var stringToEvent: Map[String, AbstractEvent] = Map.empty
  private var transactions: Map[ProgramPoint, Set[AbstractEvent]] = Map.empty
  private var curTransaction: ProgramPoint = DummyProgramPoint

  def reset(): Unit = {
    programOrder = Map.empty
    stringToEvent = Map.empty
    transactions = Map.empty
  }


  def toEventGraph[S <: State[S]]: eventgraph.Graph = {

    import eventgraph._

    Graph(
      events =
        stringToEvent.values.collect { case ProgramPointEvent(pp, obj, method) =>
          Event(pp.toString, curTransaction.toString, method)
        }.toList,
      system = TouchDevelopSystemSpecification.spec,
      programOrder =
        programOrder.map {
          case ((src, target), state) =>
            Edge(src.id, target.id, True) // TODO: Constraint
        }.toList
    )

  }

  def record[S <: State[S]](operator: String,
      this0: ExpressionSet,
      parameters: List[ExpressionSet],
      state: S,
      pp: ProgramPoint): S = {

    if (TouchAnalysisParameters.get.enableCloudAnalysis) {

      var curState = state

      for (p <- parameters) {
        for (r <- cloudPaths(curState, p)) {
          curState = recordOperation(p, r, "◈get", Nil, curState, pp)
        }
      }

      if (isEscapingOperation(operator)) {
        for (r <- cloudPaths(curState, this0)) {
          curState = recordOperation(this0, r, operator, Nil, curState, pp)
        }
      }

      curState

    } else state
  }

  private def cloudPaths[S <: State[S]](state: S, this0: ExpressionSet): List[String] = {
    state match {
      case x: TouchStateInterface[_] =>
        x.reachingHeapPaths(this0.ids) match {
          case x: SetDomain.Default.Top[List[Identifier]] => List("")
          case x: SetDomain.Default.Bottom[List[Identifier]] => Nil
          case SetDomain.Default.Inner(inner) => inner.filter(x => x.exists(isCloudIdentifier)).map(prettyPrint).toList
        }
    }
  }

  def isCloudIdentifier(value: Identifier): Boolean = value match {

    case FieldIdentifier(obj, field, typ) if obj.typ == SData =>

      val compiler = SystemParameters.compiler.asInstanceOf[TouchCompiler]
      compiler.globalData.exists(f => f.variable.getName == field && f.modifiers.contains(CloudEnabledModifier))

    case FieldIdentifier(obj, field, typ) if obj.typ == SRecords =>

      SRecords.mutableFields.exists(x => x._1.name == field && x._2.contains(CloudEnabledModifier))

    case _ =>

      false

  }

  def prettyPrint(x: List[Identifier]): String = {
    x.map {
      case FieldIdentifier(_, f, _) => f
      case VariableIdentifier(n, _) => n
      case _ => ""
    }.filter(_.nonEmpty).mkString(".")
  }

  private def isEscapingOperation(operator: String): Boolean = {
    operator match {
      case ":=" => true
      case "◈confirmed" => true
      case "◈get" => true
      case "=" => true
      case "post to wall" => true
      case "clear fields" => true
      case "equals" => true
      case _ => false
    }
  }

  private def recordOperation[S <: State[S]](this0: ExpressionSet, cloudPath: String, operator: String, parameters: List[ExpressionSet], state: S, pp: ProgramPoint): S = {

    val helpers = Singleton(SHelpers)(pp)

    val constants =
      Field[S](helpers, SHelpers.field_last_operation)(state, pp)._2 match {
        case s: SetDomain.Default.Top[Expression] =>
          SetDomain.Default.Bottom[Constant]()
        case s: SetDomain.Default.Bottom[Expression] =>
          SetDomain.Default.Bottom[Constant]()
        case SetDomain.Default.Inner(v) =>
          val separateConstants =
            for (f@FieldIdentifier(_, _, _) <- v) yield
              state.asInstanceOf[TouchStateInterface[_]].getPossibleConstants(f)

          if (separateConstants.isEmpty) SetDomain.Default.Bottom[Constant]()
          else Lattice.bigLub(separateConstants)
      }

    if (cloudPath.isEmpty) return state // HACK
    if (constants.isTop) return state // HACK

    val event = ProgramPointEvent(pp, cloudPath, operator)
    stringToEvent = stringToEvent + (event.id -> event)
    transactions = transactions + (curTransaction -> (transactions.getOrElse(curTransaction, Set.empty) + event))

    val events =
      constants.map {
        case Constant(c, _, _) if c.nonEmpty =>
          stringToEvent(c)
        case Constant(c, _, _) if c.isEmpty => // Constant may be empty when this is (potentially) the first event
          InitialEvent
      }

    // Map all actual parameters to their formal parameters
    val assignedState: S =
      (this0 :: parameters).zipWithIndex.foldLeft(state) {
        case (s, (expr, i)) =>
          val field = SHelpers.getCloudArgumentField(event.id, expr.typ.asInstanceOf[AAny], i)
          AssignField[S](helpers, field, expr)(s, pp)
      }

    // Update the stored local invariant
    if (!events.isTop) {
      for (predecessor <- events.toSetOrFail) {
        val prunedState: S =
          assignedState.pruneVariables {
            x => !x.name.startsWith("cloudarg_" + event.id) || !x.name.startsWith("cloudarg_" + predecessor.id)
          }
        programOrder = programOrder +
          ((predecessor, event) -> (programOrder.get(predecessor, event).map(_.asInstanceOf[S]).getOrElse(state.bottom()) lub prunedState))

      }
    }

    // Update state to include current relation
    val res =
      AssignField[S](
        helpers,
        SHelpers.field_last_operation,
        toRichExpression(Constant(pp.toString, TString, pp))
      )(assignedState, pp)

    res
  }

  /**
    *
    * Called whenever the analysis hits a transaction boundary.
    *
    * In TouchDevelop transaction boundaries are inserted whenever there is no code to execute, in particular,
    * when no event is currently being executed and the event queue is empty. That might happen
    *
    * (a) after an event handler terminates
    * (b) when an event handler is blocked due to an asynchronous operation.
    *
    * @param pp the program point identifying the new transaction, e.g. the program point of the
    *           event handler, or the program point after an asynchronous operation.
    *
    */
  def recordTransactionBoundary(pp: ProgramPoint): Unit = {
    curTransaction = pp
  }

  override def toString: String = {
    (for (((a, b), inv) <- programOrder) yield {
      a + "->" + b + ":" + inv
    }).mkString(",")
  }

  trait AbstractEvent {
    def id: String
  }

  case class AbstractEventWithState[S <: State[S]](aE: AbstractEvent,
      state: S) extends NodeWithState[S] {
    override def toString: String = aE.toString
  }

  case class ProgramPointEvent[S <: State[S]](pp: ProgramPoint, obj: String, method: String) extends AbstractEvent {

    val id: String = pp.toString

    override def toString: String = method

  }

  case object InitialEvent extends AbstractEvent {

    val id = ""

    override def toString: String = "Init"

  }

}
