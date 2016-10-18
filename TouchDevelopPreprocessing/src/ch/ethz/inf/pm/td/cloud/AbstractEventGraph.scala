package ch.ethz.inf.pm.td.cloud

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, WeightedGraph}
import ch.ethz.inf.pm.td.compiler.ApiMember
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._
import ch.ethz.inf.pm.td.analysis.{TouchAnalysisParameters, TouchEntryStateBuilder}
import ch.ethz.inf.pm.td.domain.{FieldIdentifier, TouchStateInterface}
import ch.ethz.inf.pm.td.semantics.{SCloud_Data, TString}

import scala.collection.mutable

/**
  * @author Lucas Brutschy
  */
object AbstractEventGraph {

  object EdgeLabel extends Enumeration {
    type EdgeLabel = Value
    val ProgramOrder, Arbitration, Dependency, AntiDependency = Value
  }

  import EdgeLabel._

  class EventGraph[S <: State[S]] extends WeightedGraph[S,EdgeLabel]

  trait AbstractEvent { def id: String }
  case object InitialEvent extends AbstractEvent { val id = "" }
  case class ProgramPointEvent(pp:ProgramPoint, method:ApiMember) extends AbstractEvent { val id = pp.toString }

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

  def toWeightedGraph[S <: State[S]]: WeightedGraph[S, EdgeLabel] = {

    val map = mutable.Map.empty[AbstractEvent,Int]
    val graph = new EventGraph[S]

    val j = graph.addNode(TouchEntryStateBuilder(TouchAnalysisParameters.get).topState.bottom().asInstanceOf[S])
    map += (InitialEvent -> j)

    for ((aE,n) <- localInvariants) {
      val i = graph.addNode(n.asInstanceOf[S])
      map += (aE -> i)
    }

    for( (a,bs) <- invProgramOrder; b <- bs.toSetOrFail ) {
      if (a != InitialEvent)
        graph.addEdge(map(a),map(b),Some(EdgeLabel.ProgramOrder))
    }

    graph
  }

  def record[S <: State[S]](event: AbstractEvent, this0: ExpressionSet, parameters: List[ExpressionSet], state: S, pp:ProgramPoint):S = {

    stringToEvent = stringToEvent + (event.id -> event)

    val cloudData = Singleton(SCloud_Data)(pp)

    val separateConstants =
      for (f@FieldIdentifier(_,_,_) <- Field[S](cloudData, SCloud_Data.field_last_operation)(state,pp).toSetOrFail) yield
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
      (event -> (events lub invProgramOrder.getOrElse(event,SetDomain.Default.Bottom[AbstractEvent]())))

    val newState:S =
      localInvariants.get(event) match {
        case Some(oldState) => oldState.asInstanceOf[S].lub(state)
        case None => state
      }

    localInvariants += (event -> newState)

    val args = this0::parameters

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
      toRichExpression(Constant(pp.toString,TString,pp))
    )(state,pp)

  }

}
