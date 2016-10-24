package ch.ethz.inf.pm.td.cloud

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.execution.NodeWithState
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, WeightedGraph}
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._
import ch.ethz.inf.pm.td.analysis.{TouchAnalysisParameters, TouchEntryStateBuilder}
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
object AbstractEventGraph {

  object EdgeLabel extends Enumeration {
    type EdgeLabel = Value
    val ProgramOrder, Arbitration, Dependency, AntiDependency = Value
  }

  import EdgeLabel._

  class EventGraph[S <: State[S]] extends WeightedGraph[NodeWithState[S],EdgeLabel]

  case class AbstractEventWithState[S <: State[S]](aE: AbstractEvent,
                                                   state:S) extends NodeWithState[S] {
    override def toString = aE.toString
  }

  trait AbstractEvent {
    def id: String
  }

  case object InitialEvent extends AbstractEvent {
    val id = ""
    override def toString:String ="Init"
  }

  case class ProgramPointEvent[S <: State[S]](pp:ProgramPoint, method:String) extends AbstractEvent {
    val id = pp.toString
    override def toString:String = method
  }

  private var invProgramOrder: Map[AbstractEvent,SetDomain.Default[AbstractEvent]] = Map.empty
  private var localInvariants: Map[AbstractEvent,_] = Map.empty
  private var arguments:       Map[AbstractEvent,List[ExpressionSet]] = Map.empty
  private var stringToEvent:   Map[String,AbstractEvent] = Map.empty

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

    val map = mutable.Map.empty[AbstractEvent,Int]
    val graph = new EventGraph[S]

    for ((aE,s) <- localInvariants) {
      val i = graph.addNode(AbstractEventWithState(aE,s.asInstanceOf[S]))
      map += (aE -> i)
    }

    for( (a,bs) <- invProgramOrder; b <- bs.toSetOrFail ) {
      if (a != InitialEvent)
        graph.addEdge(map(b),map(a),Some(EdgeLabel.ProgramOrder))
    }

    graph
  }

  def isCloudIdentifier(value: Identifier):Boolean = value match {

    case FieldIdentifier(obj,field,typ) if obj.typ == SData =>

      true

    case FieldIdentifier(obj,field,typ) if obj.typ == SRecords =>

      true

    case _ =>

      false

  }

  private def reachableFromCloud[S <: State[S]](state: S, this0: ExpressionSet): Boolean = {
    state match {
      case x:TouchStateInterface[_] =>
        x.readableFrom(this0.ids) match {
          case IdentifierSet.Top => true
          case IdentifierSet.Bottom => false
          case IdentifierSet.Inner(inner) => inner.exists(isCloudIdentifier)
        }
    }
  }

  def record[S <: State[S]](operator:String,
                            this0: ExpressionSet,
                            parameters: List[ExpressionSet],
                            state: S,
                            pp:ProgramPoint):S = {

    if (TouchAnalysisParameters.get.enableCloudAnalysis && reachableFromCloud(state,this0)) {

      val event = ProgramPointEvent(pp, operator)

      stringToEvent = stringToEvent + (event.id -> event)

      val cloudData = Singleton(SCloud_Data)(pp)

      val separateConstants =
        for (f@FieldIdentifier(_, _, _) <- Field[S](cloudData, SCloud_Data.field_last_operation)(state, pp).toSetOrFail) yield
          state.asInstanceOf[TouchStateInterface[_]].getPossibleConstants(f)

      val constants =
        if (separateConstants.isEmpty) SetDomain.Default.Bottom[Constant]()
        else Lattice.bigLub(separateConstants)

      val events =
        constants.map {
          case Constant(c, _, _) if c.nonEmpty =>
            stringToEvent(c)
          case Constant(c, _, _) if c.isEmpty => // Constant may be empty when this is (potentially) the first event
            InitialEvent
        }

      invProgramOrder = invProgramOrder +
        (event -> (events lub invProgramOrder.getOrElse(event, SetDomain.Default.Bottom[AbstractEvent]())))

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
      AssignField[S](
        Singleton(SCloud_Data)(pp),
        SCloud_Data.field_last_operation,
        toRichExpression(Constant(pp.toString, TString, pp))
      )(state, pp)

    } else state

  }

  override def toString:String = {
    "==Graph==\n" +
      (for ((aE,preds) <- invProgramOrder; pred <- preds.toSetOrFail) yield {
        pred + "->" + aE
      }).mkString(",") +
      "\n==Invariants==\n" +
      (for ((aE,inv) <- localInvariants) yield {
        aE + ":" + inv
      }).mkString(",") +
      "\n==Argument==\n" +
      (for ((aE,arg) <- arguments) yield {
        aE + ":" + arg
      }).mkString(",")
  }

}
