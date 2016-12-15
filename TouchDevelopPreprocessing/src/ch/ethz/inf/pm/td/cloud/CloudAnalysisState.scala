/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.cloud

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.execution.NodeWithState
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, WeightedGraph}
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._
import ch.ethz.inf.pm.td.analysis.{TouchAnalysisParameters, TouchEntryStateBuilder}
import ch.ethz.inf.pm.td.compiler.{CloudEnabledModifier, TouchCompiler}
import ch.ethz.inf.pm.td.domain.{FieldIdentifier, TouchStateInterface}
import ch.ethz.inf.pm.td.semantics.{SCloud_Data, SData, SRecords, TString}

import scala.collection.mutable

///**
//  * Check if something is bottom. Might be due to unreachable code.
//  */
//case class AbstractEventGraphProperty() extends SingleStatementProperty with Visitor {
//
//  def visitor = this
//
//  /**
//    * Check the property over a single state
//    *
//    * @param state     the abstract state
//    * @param statement the statement that was executed after the given abstract state
//    * @param printer   the output collector that has to be used to signal warning, validate properties, or inferred contracts
//    */
//  override def checkSingleStatement[S <: State[S]](state: S, statement: Statement, printer: OutputCollector): Unit = {
//
//    statement match {
//      case MethodCall(pp,method,parametricTypes,parameters,returnedType) =>
//
//    }
//
//  }
//
//}

/**
  * @author Lucas Brutschy
  */
object CloudAnalysisState {

  private var invProgramOrder: Map[AbstractEvent, SetDomain.Default[AbstractEvent]] = Map.empty

  import EdgeLabel._
  private var localInvariants: Map[AbstractEvent, _] = Map.empty
  private var arguments: Map[AbstractEvent, List[ExpressionSet]] = Map.empty
  private var stringToEvent: Map[String, AbstractEvent] = Map.empty

  def reset(): Unit = {
    localInvariants = Map.empty
    invProgramOrder = Map.empty
    arguments = Map.empty
    stringToEvent = Map.empty
  }

  def toWeightedGraph[S <: State[S]]: WeightedGraph[NodeWithState[S], EdgeLabel] = {

    localInvariants = localInvariants + (InitialEvent ->
      TouchEntryStateBuilder(TouchAnalysisParameters.get).topState.bottom().asInstanceOf[S]
      )

    val map = mutable.Map.empty[AbstractEvent, Int]
    val graph = new GeneralEventGraph[S]

    for ((aE, s) <- localInvariants) {
      val i = graph.addNode(AbstractEventWithState(aE, s.asInstanceOf[S]))
      map += (aE -> i)
    }

    for ((a, bs) <- invProgramOrder; b <- bs.toSetOrFail) {
      if (a != InitialEvent)
        graph.addEdge(map(b), map(a), Some(EdgeLabel.ProgramOrder))
    }

    graph
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

  //  def toBoundedGraph[S <: State[S]]: boundedgraph.Graph = {
  //
  //    import boundedgraph._
  //
  //    var events = Set.empty[Event]
  //    for ()
  //
  //
  //
  //
  //
  //    Graph(
  //      events =
  //        stringToEvent.values.map { x =>
  //          Event(x.id,x.id,
  //        },
  //      system = TouchDevelopSystemSpecification,
  //      programOrder =
  //        invProgramOrder.values.map { x =>
  //          x =>
  //
  //        }
  //    )
  //
  //  }

  private def recordOperation[S <: State[S]](this0: ExpressionSet, cloudPath: String, operator: String, parameters: List[ExpressionSet], state: S, pp: ProgramPoint): S = {

    val cloudData = Singleton(SCloud_Data)(pp)

    val constants =
      Field[S](cloudData, SCloud_Data.field_last_operation)(state, pp)._2 match {
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

    val events =
      constants.map {
        case Constant(c, _, _) if c.nonEmpty =>
          stringToEvent(c)
        case Constant(c, _, _) if c.isEmpty => // Constant may be empty when this is (potentially) the first event
          InitialEvent
      }

    invProgramOrder = invProgramOrder +
      (event -> (invProgramOrder.getOrElse(event, SetDomain.Default.Bottom[AbstractEvent]()) lub events))

    // Map all actual parameters to their formal parameters
    val assignedState: S =
    (this0 :: parameters).zipWithIndex.foldLeft(state) {
      case (s, (expr, i)) =>
        val formalArgument = ExpressionSet(VariableIdentifier("arg" + i)(expr.typ, pp))
        s.createVariable(formalArgument, expr.typ, pp).assignVariable(formalArgument, expr)
    }

    // Prune everything we do not care about.
    val prunedState: S =
    assignedState.pruneVariables {
      x => !x.name.startsWith("arg")
    }

    // Update the stored local invariant
    val newState: S =
    localInvariants.get(event) match {
      case Some(oldState) => oldState.asInstanceOf[S].lub(prunedState)
      case None => prunedState
    }
    localInvariants += (event -> newState)

    val args = this0 :: parameters

    arguments.get(event) match {
      case Some(x) =>
        assert(x.length == args.length)
        arguments += (event -> x.zip(args).map { x: (ExpressionSet, ExpressionSet) => x._1 lub x._2 })
      case None =>
        arguments += (event -> args)
    }

    // Update state to include current relation
    val res =
    AssignField[S](
      Singleton(SCloud_Data)(pp),
      SCloud_Data.field_last_operation,
      toRichExpression(Constant(pp.toString, TString, pp))
    )(state, pp)

    res
  }

  override def toString: String = {
    "==Graph==\n" +
      (for ((aE, preds) <- invProgramOrder; pred <- preds.toSetOrFail) yield {
        pred + "->" + aE
      }).mkString(",") +
      "\n==Invariants==\n" +
      (for ((aE, inv) <- localInvariants) yield {
        aE + ":" + inv
      }).mkString(",") +
      "\n==Argument==\n" +
      (for ((aE, arg) <- arguments) yield {
        aE + ":" + arg
      }).mkString(",")
  }

  trait AbstractEvent {
    def id: String
  }

  class GeneralEventGraph[S <: State[S]] extends WeightedGraph[NodeWithState[S], EdgeLabel]

  case class AbstractEventWithState[S <: State[S]](aE: AbstractEvent,
      state: S) extends NodeWithState[S] {
    override def toString: String = aE.toString
  }

  case class ProgramPointEvent[S <: State[S]](pp: ProgramPoint, obj: String, method: String) extends AbstractEvent {
    val id: String = pp.toString

    override def toString: String = method
  }

  object EdgeLabel extends Enumeration {
    type EdgeLabel = Value
    val ProgramOrder, Arbitration, Dependency, AntiDependency = Value
  }

  case object InitialEvent extends AbstractEvent {
    val id = ""

    override def toString: String = "Init"
  }

}
