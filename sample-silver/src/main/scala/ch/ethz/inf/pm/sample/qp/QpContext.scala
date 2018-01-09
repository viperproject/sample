/*
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not
  * distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.qp

import ch.ethz.inf.pm.sample.execution.{CfgPosition, CfgResult}
import viper.silver.{ast => sil}

object QpContext
  extends Context {

  val PARAMTER_PREFIX = "v"

  val QUANTIFIED_PREFIX = "q"

  val MAX_FUNCTION_NAME = "max"

  val MIN_FUNCTION_NAME = "min"

  var numerical: Map[String, CfgResult[QpParameters.NumericalState]] = Map.empty

  var invariants: Map[CfgPosition, QpSpecification] = Map.empty

  var receivers: Map[String, sil.FuncLike] = Map.empty

  var quantified: Map[String, Seq[sil.LocalVarDecl]] = Map.empty

  def setNumerical(name: String, result: CfgResult[QpParameters.NumericalState]): Unit =
    numerical = numerical.updated(name, result)

  def getNumerical: CfgResult[QpParameters.NumericalState] = method match {
    case Some(existing) => getNumerical(existing.name)
    case None => ???
  }

  def getNumerical(name: String): CfgResult[QpParameters.NumericalState] = numerical(name)

  def getReceiver: Option[sil.FuncLike] = {
    val values = receivers.values
    if (values.size <= 1) values.headOption
    else ???
  }

  def setInvariant(position: CfgPosition, invariant: QpSpecification): Unit =
    invariants = invariants.updated(position, invariant)

  def getInvariant(position: CfgPosition): QpSpecification = invariants(position)

  def getQuantified(name: String): Seq[sil.LocalVarDecl] = quantified.get(name) match {
    case Some(existing) => existing
    case None =>
      val function = getDomainFunction(name)
      val declarations = function.formalArgs.map { argument =>
        val name = uniqueIdentifier(QUANTIFIED_PREFIX, Some(0))
        sil.LocalVarDecl(name, argument.typ)()
      }
      receivers = receivers.updated(name, function)
      quantified = quantified.updated(name, declarations)
      declarations
  }

  def getMaxFunction: sil.Function = getAuxiliaryFunction(MAX_FUNCTION_NAME).get

  def getMinFunction: sil.Function = getAuxiliaryFunction(MIN_FUNCTION_NAME).get

  override protected def reset: Unit = {
    super.reset
    invariants = Map.empty
    numerical = Map.empty
    receivers = Map.empty
    quantified = Map.empty
  }

  override protected def computeFunction(name: String): Option[sil.Function] = name match {
    case MAX_FUNCTION_NAME =>
      val name0 = uniqueIdentifier(PARAMTER_PREFIX, Some(0))
      val name1 = uniqueIdentifier(PARAMTER_PREFIX, Some(1))
      val declaration0 = sil.LocalVarDecl(name0, sil.Perm)()
      val declaration1 = sil.LocalVarDecl(name1, sil.Perm)()
      val variable0 = sil.LocalVar(name0)(sil.Perm)
      val variable1 = sil.LocalVar(name1)(sil.Perm)
      val function = sil.Function(
        name = uniqueIdentifier(name),
        formalArgs = Seq(declaration0, declaration1),
        typ = sil.Perm,
        pres = Seq.empty,
        posts = Seq.empty,
        decs = None,
        body = Some(sil.CondExp(sil.PermGtCmp(variable0, variable1)(), variable0, variable1)())
      )()
      Some(function)
    case MIN_FUNCTION_NAME =>
      val name0 = uniqueIdentifier(PARAMTER_PREFIX, Some(0))
      val name1 = uniqueIdentifier(PARAMTER_PREFIX, Some(1))
      val declaration0 = sil.LocalVarDecl(name0, sil.Perm)()
      val declaration1 = sil.LocalVarDecl(name1, sil.Perm)()
      val variable0 = sil.LocalVar(name0)(sil.Perm)
      val variable1 = sil.LocalVar(name1)(sil.Perm)
      val function = sil.Function(
        name = uniqueIdentifier(name),
        formalArgs = Seq(declaration0, declaration1),
        typ = sil.Perm,
        pres = Seq.empty,
        posts = Seq.empty,
        decs = None,
        body = Some(sil.CondExp(sil.PermGtCmp(variable0, variable1)(), variable1, variable0)())
      )()
      Some(function)
    case _ => super.computeFunction(name)
  }
}

trait Context {
  protected var program: Option[sil.Program] = None

  protected var method: Option[sil.Method] = None

  protected var identifiers: Set[String] = Set.empty

  protected var functions: Map[String, sil.Function] = Map.empty

  def setProgram(current: sil.Program): Unit = program match {
    case Some(`current`) => // do nothing
    case _ =>
      reset
      program = Some(current)

      identifiers = identifiers ++ current.fields.map(_.name)
      identifiers = identifiers ++ current.domains.map(_.name)
      identifiers = identifiers ++ current.functions.map(_.name)
      identifiers = identifiers ++ current.methods.map(_.name)

      identifiers = identifiers ++ current.methods.flatMap { method =>
        val arguments = method.formalArgs.map(_.name)
        val returns = method.formalReturns.map(_.name)
        val declarations = method.body.toSeq.flatMap { body => body.scopedDecls.map(_.name) }
        arguments ++ returns ++ declarations
      }

      identifiers = identifiers ++ current.domains.flatMap { domain =>
        val functions = domain.functions.map(_.name)
        val axioms = domain.axioms.map(_.name)
        functions ++ axioms
      }
  }

  def setMethod(name: String): Unit =
    method = program.flatMap(_.methods.find(_.name == name))

  def getFunction(name: String): Option[sil.FuncLike] = program.flatMap { program => program.findFunctionOptionally(name).orElse(program.findDomainFunctionOptionally(name)) }

  def getAuxiliaryFunction(name: String): Option[sil.Function] = functions.get(name) match {
    case Some(existing) => Some(existing)
    case None =>
      val function = computeFunction(name)
      function.foreach { function => functions = functions.updated(name, function) }
      function
  }

  def getDomainFunction(name: String): sil.DomainFunc =
    program.get.findDomainFunction(name)

  protected def reset: Unit = {
    program = None
    method = None
    identifiers = Set.empty
    functions = Map.empty
  }

  protected def computeFunction(name: String): Option[sil.Function] = None

  protected def uniqueIdentifier(name: String, n: Option[Int] = None): String = {
    val identifier = if (n.isDefined || identifiers.contains(name)) {
      var count = n.getOrElse(0)
      while (identifiers.contains(name + count)) count = count + 1
      name + count
    } else name
    identifiers = identifiers + identifier
    identifier
  }
}
