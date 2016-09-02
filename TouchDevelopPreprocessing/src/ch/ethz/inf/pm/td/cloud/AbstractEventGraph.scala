package ch.ethz.inf.pm.td.cloud

import ch.ethz.inf.pm.sample.abstractdomain.{Constant, ExpressionSet, SetDomain, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.ApiMember
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._
import ch.ethz.inf.pm.td.semantics.{SCloud_Data, TString}

/**
  * @author Lucas Brutschy
  */
object AbstractEventGraph {

  case class AbstractEvent(pp:ProgramPoint, method:ApiMember)

  private var invProgramOrder: Map[AbstractEvent,SetDomain.Default[AbstractEvent]] = Map.empty
  private var localInvariants: Map[AbstractEvent,_] = Map.empty
  private var arguments:       Map[AbstractEvent,List[ExpressionSet]] = Map.empty

  def reset(): Unit = {
    localInvariants = Map.empty
    invProgramOrder = Map.empty
    arguments = Map.empty
  }

  def record[S <: State[S]](event: AbstractEvent, this0: ExpressionSet, parameters: List[ExpressionSet], state: S, pp:ProgramPoint):S = {

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
