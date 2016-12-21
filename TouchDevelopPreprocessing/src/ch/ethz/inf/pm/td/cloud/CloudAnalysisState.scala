/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.cloud

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{State, _}
import ch.ethz.inf.pm.sample.execution.NodeWithState
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, ProgramPoint, Type}
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._
import ch.ethz.inf.pm.td.analysis.TouchAnalysisParameters
import ch.ethz.inf.pm.td.compiler.{CloudEnabledModifier, TouchCompiler}
import ch.ethz.inf.pm.td.domain.{FieldIdentifier, TouchStateInterface}
import ch.ethz.inf.pm.td.semantics._

/**
  * @author Lucas Brutschy
  */
object CloudAnalysisState {

  private var localInvariants: Map[AbstractEvent, _] = Map.empty
  private var invProgramOrder: Map[AbstractEvent, Set[AbstractEvent]] = Map.empty
  private var stringToEvent: Map[String, AbstractEvent] = Map.empty
  private var curTransaction: ProgramPoint = DummyProgramPoint

  def reset(): Unit = {
    localInvariants = Map.empty
    invProgramOrder = Map.empty
    stringToEvent = Map.empty
    curTransaction = DummyProgramPoint
  }

  def toEventGraph[S <: State[S]]: eventgraph.Graph = {

    import eventgraph._

    def extractConstraints(state: S, src: String, tgt: String): eventgraph.Expr = {

      val helpers = Singleton(SHelpers)(DummyProgramPoint)
      val fields = SHelpers.getCloudArgumentFields(src)(state) ++ SHelpers.getCloudArgumentFields(tgt)(state)
      val identifiers = fields.flatMap(x => state.getFieldValue(helpers, x.getName, x.typ).expr.ids.getNonTopUnsafe)

      convert(
        state.asInstanceOf[TouchStateInterface[_]].getConstraints(identifiers),
        src, tgt)
    }

    Graph(
      events =
        stringToEvent.values.collect { case ProgramPointEvent(pp, obj, method, txn) =>
          Event(pp.toString, txn.toString, method)
        }.toList,
      system = TouchDevelopSystemSpecification.spec,
      programOrder =
        invProgramOrder.flatMap {
          case (src, targets) =>

            val state = localInvariants(src).asInstanceOf[S]

            for (target <- targets) yield {
              Edge(src.id, target.id, extractConstraints(state, src.id, target.id))
            }

        }.toList
    )

  }

  def convert(expressions: Set[Expression], src: String, tgt: String): eventgraph.Expr = {

    import eventgraph._

    def convert(expr: Expression): Expr = {
      expr match {
        case NegatedBooleanExpression(exp) =>
          Not(convert(exp))
        case BinaryBooleanExpression(left, right, op) =>
          val (l, r) = (convert(left), convert(right))
          op match {
            case BooleanOperator.&& => And(l, r)
            case BooleanOperator.|| => Or(l, r)
            case _ => ???
          }
        case BinaryArithmeticExpression(left, right, op) =>
          val (l, r) = (convert(left), convert(right))
          op match {
            case ArithmeticOperator.== => Equal(l, r)
            case ArithmeticOperator.!= => Unequal(l, r)
          }
        case UnaryArithmeticExpression(left, op, _) => ???
        case Constant(constant, typ, _) if typ.isStringType =>
          StringConst(constant)
        case Constant("true", typ, _) if typ.isBooleanType =>
          True
        case Constant("true", typ, _) if typ.isBooleanType =>
          False
        case Constant(v, typ, _) if typ.isNumericalType =>
          IntConst(v.toInt)
        case FieldIdentifier(_, name, _) =>
          val (eventID, typ, num) = CloudArgumentFieldNames.deconstruct(name)
          typ match {
            case "Number" if src == eventID => IntArgLeft(num)
            case "Number" if tgt == eventID => IntArgRight(num)
            case "Boolean" if src == eventID => BoolArgLeft(num)
            case "Boolean" if tgt == eventID => BoolArgRight(num)
            case "String" if src == eventID => StringArgLeft(num)
            case "String" if tgt == eventID => StringArgRight(num)
          }
        case _ =>
          throw new UnsupportedOperationException("This expression cannot be converted into SMT syntax")
      }
    }

    BigAnd(expressions.map(convert).toList)
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
          case _: SetDomain.Default.Top[List[Identifier]] => List("")
          case _: SetDomain.Default.Bottom[List[Identifier]] => Nil
          case SetDomain.Default.Inner(inner) => inner.filter(x => x.exists(isCloudIdentifier)).map(prettyPrint).toList
        }
    }
  }

  def isCloudIdentifier(value: Identifier): Boolean = value match {

    case FieldIdentifier(obj, field, _) if obj.typ == SData =>

      val compiler = SystemParameters.compiler.asInstanceOf[TouchCompiler]
      compiler.globalData.exists(f => f.variable.getName == field && f.modifiers.contains(CloudEnabledModifier))

    case FieldIdentifier(obj, field, _) if obj.typ == SRecords =>

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
        case _: SetDomain.Default.Top[Expression] =>
          SetDomain.Default.Bottom[Constant]()
        case _: SetDomain.Default.Bottom[Expression] =>
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

    val event = ProgramPointEvent(pp, cloudPath, operator, curTransaction)
    stringToEvent = stringToEvent + (event.id -> event)

    val events =
      constants.map {
        case Constant(c, _, _) if c.nonEmpty =>
          Some(stringToEvent(c))
        case Constant(c, _, _) if c.isEmpty => // Constant may be empty when this is (potentially) the first event
          None
      }

    // Map all actual parameters to their formal parameters
    val assignedState: S =
      (this0 :: parameters).zipWithIndex.foldLeft(state) {
        case (s, (expr, i)) =>
          val field = SHelpers.getCloudArgumentField(event.id, expr.typ.asInstanceOf[AAny],
            CloudArgumentFieldNames.make(event.id, expr.typ, i))
          AssignField[S](helpers, field, expr)(s, pp)
      }

    val prunedState: S =
      assignedState.pruneVariables {
        x => x.name != SHelpers.name.toLowerCase
      }
    localInvariants = localInvariants +
      (event -> (localInvariants.get(event).map(_.asInstanceOf[S]).getOrElse(state.bottom()) lub prunedState))

    // Update the stored local invariant
    if (!events.isTop) {
      invProgramOrder = invProgramOrder +
        (event -> (invProgramOrder.getOrElse(event, Set.empty) ++ events.toSetOrFail.flatten))
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
    "==Graph==\n" +
      (for ((aE, preds) <- invProgramOrder; pred <- preds) yield {
        pred + "->" + aE
      }).mkString(",") +
      "\n==Invariants==\n" +
      (for ((aE, inv) <- localInvariants) yield {
        aE + ":" + inv
      }).mkString(",")
  }

  trait AbstractEvent {
    def id: String
  }

  case class AbstractEventWithState[S <: State[S]](aE: AbstractEvent,
      state: S) extends NodeWithState[S] {
    override def toString: String = aE.toString
  }

  case class ProgramPointEvent[S <: State[S]](pp: ProgramPoint, obj: String, method: String, txn: ProgramPoint) extends AbstractEvent {

    val id: String = pp.toString

    override def toString: String = method

  }

  object CloudArgumentFieldNames {

    def make(eventID: String, typ: Type, num: Int): String =
      "cloudarg_" + eventID + "_" + typ.name + "_" + num

    def isValid(str: String): Boolean = {
      str.startsWith("cloudarg_")
    }

    def deconstruct(str: String): (String, String, Int) = {
      val Array(eventId, typName, num) = str.stripPrefix("cloudarg_").split("_")
      (eventId, typName, num.toInt)
    }

  }

  // TODO: Do we need an initial event?
  //  case object InitialEvent extends AbstractEvent {
  //
  //    val id = "init"
  //
  //    override def toString: String = "Init"
  //
  //  }

}
