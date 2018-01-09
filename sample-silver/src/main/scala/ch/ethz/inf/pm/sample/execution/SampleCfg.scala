/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.execution.SampleCfg.{SampleBlock, SampleEdge}
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.oorepresentation.silver.sample.ProgramPoint
import viper.silver.cfg._
import viper.silver.cfg.utility.LoopDetector

/**
  * A sample control flow graph
  *
  * @author Jerome Dohrau
  */
class SampleCfg(val blocks: Seq[SampleBlock], val edges: Seq[SampleEdge], val entry: SampleBlock, val exit: Option[SampleBlock])
  extends Cfg[Statement, Statement] {

  private lazy val positions: Map[ProgramPoint, CfgPosition] = {
    // generate all block positions
    val blockPositions = for (block <- blocks; (element, index) <- block.elements.map(_.merge).zipWithIndex) yield {
      val pp = ProgramPointUtils.identifyingPP(element)
      val position = BlockPosition(block, index)
      pp -> position
    }
    // generate all edge positions
    val edgePositions = edges.flatMap {
      case edge@ConditionalEdge(condition, _, _, _) =>
        val pp = ProgramPointUtils.identifyingPP(condition)
        val position = EdgePosition(edge)
        Some(pp -> position)
      case _ => None
    }
    // return map
    (blockPositions ++ edgePositions).toMap
  }

  def getPosition(pp: ProgramPoint): CfgPosition = positions(pp)

  def getBlockPosition(pp: ProgramPoint): BlockPosition = getPosition(pp) match {
    case position: BlockPosition => position
    case _ => throw new IllegalArgumentException(s"The program point $pp does not refer to a block position.")
  }

  def getEdgePosition(pp: ProgramPoint): EdgePosition = getPosition(pp) match {
    case position: EdgePosition => position
    case _ => throw new IllegalArgumentException(s"The program point $pp does not refer to an edge position.")
  }

  def changingVariables(block: SampleBlock): Seq[VariableIdentifier] = {
    val backedges = inEdges(block).filterNot(_.isIn)
    val blocks = backedges.flatMap { backedge => LoopDetector.collectBlocks(this, backedge) }
    val variables = blocks.distinct.flatMap { block => writtenVariables(block) }
    variables.distinct
  }

  def writtenVariables(block: SampleBlock): Seq[VariableIdentifier] = block match {
    case LoopHeadBlock(_, statements) => statements.flatMap(writtenVariables)
    case StatementBlock(statements) => statements.flatMap(writtenVariables)
    case _ => ???
  }

  def writtenVariables(statement: Statement): Seq[VariableIdentifier] = statement match {
    case Assignment(_, Variable(_, identifier), _) => Seq(identifier)
    case _ => Seq.empty
  }

  override def copy(blocks: Seq[SampleBlock] = blocks,
                    edges: Seq[SampleEdge] = edges,
                    entry: SampleBlock = entry,
                    exit: Option[SampleBlock] = exit): SampleCfg =
    SampleCfg(blocks, edges, entry, exit)
}

object SampleCfg {
  type SampleBlock = Block[Statement, Statement]
  type SampleEdge = Edge[Statement, Statement]

  def apply(): SampleCfg = {
    val entry: SampleBlock = PreconditionBlock(Seq.empty)
    val exit: SampleBlock = PostconditionBlock(Seq.empty)
    val blocks = Seq(entry, exit)
    val edges = Seq(UnconditionalEdge(entry, exit))
    SampleCfg(blocks, edges, entry, Some(exit))
  }

  def apply(blocks: Seq[SampleBlock], edges: Seq[SampleEdge], entry: SampleBlock, exit: Option[SampleBlock]): SampleCfg =
    new SampleCfg(blocks, edges, entry, exit)

  def unapply(cfg: SampleCfg): Option[(Seq[SampleBlock], Seq[SampleEdge], SampleBlock, Option[SampleBlock])] =
    Some((cfg.blocks, cfg.edges, cfg.entry, cfg.exit))
}
