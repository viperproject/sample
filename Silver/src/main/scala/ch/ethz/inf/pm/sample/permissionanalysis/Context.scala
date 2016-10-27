package ch.ethz.inf.pm.sample.permissionanalysis

import ch.ethz.inf.pm.sample.execution.TrackingCFGState
import ch.ethz.inf.pm.sample.oorepresentation.{CFGPosition, ProgramPoint}

/**
  *  A context object for the permission inference that stores the result of the
  * alias analysis.
  *
  * @author Jerome Dohrau
  */
object Context {
  /**
    * Stores the result of the alias analysis.
    */
  private var aliases: Option[TrackingCFGState[_ <: AliasAnalysisState[_]]] = None

  /**
    * Sets the result of the alias analysis.
    *
    * @param aliases The result of the alias analysis to set.
    * @tparam A The type of the alias analysis.
    */
  def setAliases[A <: AliasAnalysisState[A]](aliases: TrackingCFGState[A]) = {
    this.aliases = Some(aliases)
  }

  /**
    * Clears the result of the alias analysis.
    */
  def clearAliases(): Unit = {
    this.aliases = None
  }

  /**
    * Returns the state of the alias analysis before the given program point.
    *
    * @param pp The program point.
    * @tparam A The type of the alias analysis.
    * @return The state of the alias analysis before the given program point.
    */
  def preAliases[A <: AliasAnalysisState[A]](pp: ProgramPoint): A =
    aliases.get.preStateAt(position(pp)).asInstanceOf[A]

  /**
    * Returns the state of the alias analysis after the given program point.
    *
    * @param pp The program point.
    * @tparam A The type fo the alias analysis.
    * @return The state of the alias analysis after the given program point.
    */
  def postAliases[A <: AliasAnalysisState[A]](pp: ProgramPoint): A =
    aliases.get.postStateAt(position(pp)).asInstanceOf[A]

  /**
    * Returns the cfg position corresponding to the given program point.
    *
    * @param pp The program point.
    * @return The cfg position corresponding to the given program point.
    */
  private def position(pp: ProgramPoint): CFGPosition = {
    val cfg = aliases.get.cfg
    val positions = for {
      (block, i) <- cfg.nodes.zipWithIndex
      (statement, j) <- block.zipWithIndex
      if statement.getPC() == pp
    } yield CFGPosition(i, j)
    positions.head
  }
}
