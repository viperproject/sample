package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.NumericalDomain
import ch.ethz.inf.pm.sample.execution.TrackingCFGState
import ch.ethz.inf.pm.sample.oorepresentation.silver.DefaultSilverConverter
import ch.ethz.inf.pm.sample.oorepresentation.{CFGPosition, ProgramPoint}
import ch.ethz.inf.pm.sample.permissionanalysis.AliasAnalysisState
import viper.silver.ast.{CondExp, Function, LocalVar, LocalVarDecl, NoPerm, Perm, PermGtCmp, PermLtCmp, Program}

import scala.collection._

/**
  * Created by severin on 29.10.16.
  */
object Context {

  lazy val prog: Program = DefaultSilverConverter.prog

  val identifiers: mutable.Set[String] = mutable.Set()

  val auxiliaryFunctions: mutable.Set[Function] = mutable.Set()

  var maxFunction: Option[Function] = None

  var boundaryFunction: Option[Function] = None

  def initContext = {
    // Add all existing identifiers to the identifiers set (fields, domain names, method names, function names etc.)
    identifiers ++= prog.fields.map(field => field.name)
    identifiers ++= prog.methods.flatMap(method => (method.formalArgs ++ method.formalReturns).toSet).map(varDecl => varDecl.name)
    identifiers ++= prog.methods.map(method => method.name)
    identifiers ++= prog.domains.map(domain => domain.name)
    identifiers ++= prog.functions.map(function => function.name)
    identifiers ++= prog.predicates.map(predicates => predicates.name)
    identifiers ++= prog.domains.flatMap(domain => domain._axioms.map(axiom => axiom.name) ++ domain._functions.map(function => function.name))
  }

  def createNewUniqueVarIdentifier = {
    val prefix = "_var"
    var count = 0
    while (identifiers.contains(prefix + count)) {
      count += 1
    }
    val identifier = prefix + count
    identifiers += identifier
    identifier
  }

  def createNewUniqueFunctionIdentifier = {
    val prefix = "_func"
    var count = 0
    while (identifiers.contains(prefix + count)) {
      count += 1
    }
    val identifier = prefix + count
    identifiers += identifier
    identifier
  }

  def getMaxFunction = maxFunction match {
    case Some(existingMaxFunction) => existingMaxFunction
    case None =>
      val fun = Function(createNewUniqueFunctionIdentifier, Seq(VarXDecl, VarYDecl), Perm, Seq(), Seq(),
        Some(CondExp(PermGtCmp(VarX, VarY)(), VarX, VarY)())
      )()
      maxFunction = Some(fun)
      auxiliaryFunctions += fun
      fun
  }

  def getBoundaryFunction = boundaryFunction match {
    case Some(existingMaxFunction) => existingMaxFunction
    case None =>
      val fun = Function(createNewUniqueFunctionIdentifier, Seq(VarXDecl), Perm, Seq(), Seq(),
        Some(CondExp(PermLtCmp(VarX, ZeroPerm)(), VarX, ZeroPerm)())
      )()
      boundaryFunction= Some(fun)
      auxiliaryFunctions += fun
      fun
  }

  /**
    * Stores the result of the alias analysis.
    */
  private var aliases: Option[TrackingCFGState[_ <: AliasAnalysisState[_]]] = None

  /**
    * Stores the result of the numerical analysis.
    */
  private var numericalInfo: Option[TrackingCFGState[_ <: NumericalAnalysisState[_ <: NumericalDomain[_], _]]] = None

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
    * Sets the result of the numerical analysis.
    *
    * @param numericalInfo The result of the numerical analysis to set.
    * @tparam T The type of the numerical analysis.
    */
  def setNumericalInfo[N <: NumericalDomain[N], T <: NumericalAnalysisState[N, T]](numericalInfo: TrackingCFGState[T]) = {
    this.numericalInfo = Some(numericalInfo)
  }

  /**
    * Clears the result of the numerical analysis.
    */
  def clearNumericalInfo(): Unit = {
    this.numericalInfo = None
  }

  /**
    * Returns the state of the numerical analysis before the given program point.
    *
    * @param pp The program point.
    * @tparam T The type of the numerical analysis.
    * @return The state of the numerical analysis before the given program point.
    */
  def preNumericalInfo[N <: NumericalDomain[N], T <: NumericalAnalysisState[N, T]](pp: ProgramPoint): T =
  numericalInfo.get.preStateAt(position(pp)).asInstanceOf[T]

  /**
    * Returns the state of the alias analysis after the given program point.
    *
    * @param pp The program point.
    * @tparam T The type fo the numerical analysis.
    * @return The state of the alias analysis after the given program point.
    */
  def postNumericalInfo[N <: NumericalDomain[N], T <: NumericalAnalysisState[N, T]](pp: ProgramPoint): T =
  numericalInfo.get.postStateAt(position(pp)).asInstanceOf[T]

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

object VarXDecl extends LocalVarDecl("x", Perm)()

object VarX extends LocalVar("x")(Perm)

object VarYDecl extends LocalVarDecl("y", Perm)()

object VarY extends LocalVar("y")(Perm)

object ZeroPerm extends NoPerm()()