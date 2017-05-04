/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State, UtilitiesOnStates, VariableIdentifier}
import ch.ethz.inf.pm.sample.execution.SampleCfg.SampleEdge
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.oorepresentation.silver.{SilverMethodDeclaration, SilverProgramDeclaration}
import ch.ethz.inf.pm.sample.permissionanalysis.LeaveMethodCommand
import com.typesafe.scalalogging.LazyLogging
import viper.silver.cfg.{LoopHeadBlock, StatementBlock}

import scala.collection.mutable

/**
  * The interprocedural interpreter adds auxiliary edges to the CFG.
  * This trait represents such an edge.
  */
sealed trait AuxiliaryEdge

/**
  * A dummy edge used to annotate the CFG with additional information.
  *
  * @param info can be used to provide more info about this edge
  * @tparam T type of the info attribute
  */
case class DummyEdge[T](info: T) extends AuxiliaryEdge

/**
  * A method return edges points from a called method to the next instruction after the method call. These
  * edges can be used to merge the "effect of the method" into the callers state.
  *
  * The exitState will contain the result of the method that was called in the previous instruction. This information
  * can be merged into the current state. E.g. using this: (State[S]).command(LeaveMethodCommand(exitState))
  *
  * @param exitState the exitState() for the called method
  * @tparam S the domain used in the state
  */
case class MethodReturnEdge[S](exitState: S) extends AuxiliaryEdge

/**
  * Represents a method call. Blocks with in-edges of type MethodCallEdge are called from somewhere in the
  * analyzed program. The inputState is a state where the parameters have been initialised using the arguments
  * from the calling context.
  *
  * @param inputState a method declaration of the called method.
  */
case class MethodCallEdge[S](inputState: S) extends AuxiliaryEdge

/**
  * Performs a forward interpretation of a control flow graph with special handling for method and function calls.
  *
  * @tparam S The type of the states.
  * @author Flurin Rindisbacher
  */
trait InterproceduralSilverForwardInterpreter[S <: State[S]]
  extends SilverForwardInterpreter[S]
    with LazyLogging {
  val program: SilverProgramDeclaration
  val builder: SilverEntryStateBuilder[S]
  val methodEntryStates: mutable.Map[String, mutable.Map[ProgramPoint, S]] = mutable.Map().withDefault(_ => mutable.Map())
  val methodExitStates: mutable.Map[String, S] = mutable.Map()
  val callsInProgram: Map[String, Set[BlockPosition]]
  val programResult: ProgramResult[S] = DefaultProgramResult(program)

  def executeInterprocedural(startCfg: SampleCfg, initial: S): ProgramResult[S] = {
    super.execute(startCfg, initial)
    programResult
  }

  override protected def inEdges(current: BlockPosition, cfgResult: Map[SampleCfg, CfgResult[S]]): Seq[Either[SampleEdge, AuxiliaryEdge]] = {
    /**
      * If the current block is an entrypoint of the cfg and this block (method) is called throughout the program
      * we'll add a MethodCallEdge
      *
      * @return
      */
    def createMethodCallEdges(): Seq[Either[SampleEdge, MethodCallEdge[S]]] = {
      if (cfg(current).entry != current.block)
        return Seq.empty
      val method = program.methods.find(_.body == cfg(current)).head
      if (callsInProgram.contains(method.name.name)) {
        (for(entryState <- methodEntryStates(method.name.name).values) yield{
          Right(MethodCallEdge(entryState))
        }).toList
      } else {
        Seq.empty
      }
    }

    current match {
      case BlockPosition(_, 0) => super.inEdges(current, cfgResult) ++ createMethodCallEdges()
      case BlockPosition(block, index) => {
        val previousStatement = block.elements(index - 1).merge
        previousStatement match {
          case MethodCall(pp, method: Variable, _, _, _, _) => Seq(Right(MethodReturnEdge(cfgResult(findMethod(method.getName).body).exitState())))
          case _ => ???
        }
      }
    }
  }

  override protected def onExitBlockExecuted(current: BlockPosition, worklist: mutable.Queue[BlockPosition]): Unit = {
    val method = findMethod(current)
    val name = method.name.name
    callsInProgram(name)
      .filter(b => methodEntryStates(name)
        // block must have been analyzed before and methodcall mustn't be the last statement
        .contains(b.block.elements(b.index).merge.getPC()) && b.index < b.block.elements.size - 1
      )
      .foreach(b => worklist.enqueue(BlockPosition(b.block, b.index + 1)))
  }

  override protected def cfg(blockPosition: BlockPosition): SampleCfg = {
    program.methods.filter(_.body.blocks.contains(blockPosition.block)).head.body
  }

  private def findMethod(blockPosition: BlockPosition): SilverMethodDeclaration = {
    program.methods.filter(_.body.blocks.contains(blockPosition.block)).head
  }

  private def findMethod(name: String): SilverMethodDeclaration = {
    program.methods.find(_.name.name == name).get
  }

  override def getPredecessorState(cfgResult: CfgResult[S], current: BlockPosition, edge: Either[SampleEdge, AuxiliaryEdge]): S = edge match {
    //
    // if previous instruction was a method-call and we hava a BlockPosition.index > 0 then it was a back-jump from
    // a method-call. Use the entrystate and add the infos we got from analyzing the called method.
    //
    case Right(MethodCallEdge(callingContext: S)) => {
      val methodDeclaration = findMethod(current)
      val name = methodDeclaration.name.name
      val tmpArguments = for ((param, index) <- methodDeclaration.arguments.zipWithIndex) yield {
        ExpressionSet(VariableIdentifier("arg_#" + index)(param.typ))
      }
      var inputState = builder.build(program, methodDeclaration) lub callingContext
      // assign (temporary) arguments to parameters and remove the temp args
      inputState = methodDeclaration.arguments.zip(tmpArguments).foldLeft(inputState)((st, tuple) => st.assignVariable(ExpressionSet(tuple._1.variable.id), tuple._2))
      tmpArguments.foldLeft(inputState)((st, tmpArg) => st.removeVariable(tmpArg))
    }
    case Right(MethodReturnEdge(exitState)) => {
      val tmp = cfgResult.preStateAt(current)
      current.block match {
        case StatementBlock(statements) => statements(current.index - 1) match {
          case MethodCall(_, v: Variable, _, _, _, targets: List[Statement]) => {
            val methodDeclaration = program.methods.find(m => m.name.name == v.getName).get
            val methodCall = statements(current.index - 1).asInstanceOf[MethodCall]
            tmp.command(LeaveMethodCommand(methodDeclaration, methodCall, exitState))
          }
          case _ => tmp
        }
        case LoopHeadBlock(invs, statements) => statements(current.index - invs.size - 1) match {
          case MethodCall(_, v: Variable, _, _, _, targets: List[Statement]) => {
            val methodDeclaration = program.methods.find(m => m.name.name == v.getName).get
            val methodCall = statements(current.index - 1).asInstanceOf[MethodCall]
            tmp.command(LeaveMethodCommand(methodDeclaration, methodCall, exitState))
          }
          case _ => tmp
        }
        case _ => tmp
      }
    }
    case _ => super.getPredecessorState(cfgResult, current, edge)
  }

  override protected def executeStatement(statement: Statement, state: S, worklist: mutable.Queue[BlockPosition]): (Boolean, S) = {
    statement match {
      case MethodCall(_, f: FieldAccess, _, _, _, _) => return super.executeStatement(statement, state, worklist)
      case call: MethodCall => {
        val predecessor = state.before(ProgramPointUtils.identifyingPP(statement))
        val name: String = call.method match {
          case v: Variable => v.getName
          case _ => throw new RuntimeException("Should not happen") //TODO
        }
        var currentState = predecessor
        val targetExpressions = for (target <- call.targets) yield {
          val (exp, st) = UtilitiesOnStates.forwardExecuteStatement(currentState, target)
          currentState = st
          exp
        }
        val parameterExpressions = for (parameter <- call.parameters) yield {
          currentState = parameter.forwardSemantics[S](currentState)
          currentState.expr
        }
        val methodDeclaration = program.methods.find(m => m.name.name == name).get

        // create arg_# variables and assign the value to them. then remove all non arg_# variables
        var tmpVariableState = currentState
        val tmpArguments = for ((param, index) <- parameterExpressions.zipWithIndex) yield {
          val exp = ExpressionSet(VariableIdentifier("arg_#" + index)(param.typ))
          tmpVariableState = tmpVariableState.createVariable(exp, param.typ, DummyProgramPoint)
          tmpVariableState = tmpVariableState.assignVariable(exp, param)
          exp
        }
        tmpVariableState = tmpVariableState.ids.toSetOrFail // let's remove them
          .filter(id => !id.getName.startsWith("arg_#"))
          .foldLeft(tmpVariableState)((st, ident) => st.removeVariable(ExpressionSet(ident)))

        //context insensitive analysis: analyze the called method with the join of all calling states
        // this implementation could analyze the method several times
        methodEntryStates(name) = methodEntryStates(name) + (statement.getPC() -> tmpVariableState)
        worklist.enqueue(BlockPosition(methodDeclaration.body.entry, 0))
        //        logger.trace(predecessor.toString)
        //        logger.trace(statement.toString)
        //        logger.trace(resultState.toString)
        (false, currentState)
      }
      case _ => return super.executeStatement(statement, state, worklist)
    }
  }
}

case class FinalResultInterproceduralForwardInterpreter[S <: State[S]](override val program: SilverProgramDeclaration, override val builder: SilverEntryStateBuilder[S], override val callsInProgram: Map[String, Set[BlockPosition]])
  extends InterproceduralSilverForwardInterpreter[S] {
  override protected def initializeResult(cfg: SampleCfg, state: S): CfgResult[S] = ???

  override protected def initializeResultForward(cfg: SampleCfg, state: S): Map[SampleCfg, CfgResult[S]] = {
    programResult.initialize((c, st) => {
      val cfgResult = FinalCfgResult[S](c)
      cfgResult.initialize(state)
      cfgResult
    }, state)
    (for (method <- program.methods) yield {
      (method.body -> programResult.getResult(method.name))
    }).toMap
  }
}