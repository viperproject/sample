/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.execution.CfgResult
import ch.ethz.inf.pm.sample.oorepresentation.silver._
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.QuantifiedPermissionsParameters._
import viper.silver.ast.Function
import viper.silver.cfg.LoopHeadBlock
import viper.silver.{ast => sil}

import scala.collection.GenTraversableOnce

/**
  * @author Jerome Dohrau
  * @author Severin Münger
  */
object Context {

  /**
    * The program that is currently analyzed.
    */
  private var program: sil.Program = _

  /**
    * The method that is currently analyzed.
    */
  private var method: sil.Method = _

  /**
    * The set of identifiers.
    */
  private var identifiers: Set[String] = Set()

  /**
    * Maps name of domain functions to the sequence of the corresponding
    * quantified variables.
    */
  private var receivers: Map[String, Seq[VariableIdentifier]] = Map()

  /**
    * The map containing all auxiliary functions.
    */
  private var functions: Map[String, sil.Function] = Map()

  /**
    * Sets the program that is currently analyzed to the given program.
    *
    * @param program The program.
    */
  def setProgram(program: sil.Program): Unit = {
    this.program = program
    // Add all existing identifiers to the identifiers set (fields, domain
    // names, method names, function names etc.)
    identifiers ++= program.fields.map(_.name)
    identifiers ++= program.methods.flatMap(_.formalArgs.map(_.name))
    identifiers ++= program.methods.flatMap(_.formalReturns.map(_.name))
    identifiers ++= program.methods.flatMap(_.body.scopedDecls.map(_.name))
    identifiers ++= program.methods.map(_.name)
    identifiers ++= program.functions.map(_.name)
    identifiers ++= program.predicates.map(_.name)
    identifiers ++= program.domains.map(_.name)
    identifiers ++= program.domains.flatMap(_.functions.map(_.name))
    identifiers ++= program.domains.flatMap(_.axioms.map(_.name))
  }

  /**
    * Selects the method with the given method as the method that is currently
    * analyzed.
    *
    * @param name The name of the method.
    */
  def selectMethod(name: String): Unit =
    program.methods.find(_.name == name) match {
      case Some(existing) => method = existing
      case _ => ???
    }

  /**
    * Returns the function with the given name. This method first, searches
    * through the auxiliary functions, then the program functions, and finally
    * the domain functions.
    *
    * @param name The name of the function.
    * @return The function.
    */
  def getFunction(name: String): sil.FuncLike =
    functions.get(name)
      .orElse(program.findFunctionOptionally(name))
      .getOrElse(program.findDomainFunction(name))

  def getVariables(name: String): Seq[VariableIdentifier] = {
    receivers.get(name) match {
      case Some(existing) => existing
      case None =>
        val function = getFunction(name)
        val quantified = function.formalArgs.map { argument =>
          val name = uniqueIdentifier("q")
          val typ = DefaultSilverConverter.convert(argument.typ)
          VariableIdentifier(name)(typ)
        }
        receivers += (name -> quantified)
        quantified
    }
  }

  def getReceiver: sil.FuncLike = {
    val names = receivers.keys
    if (names.size == 1) getFunction(names.head)
    else ???
  }

  /* ------------------------------------------------------------------------- *
   * Numerical Information
   */

  private var numerical: Map[String, CfgResult[NumericalStateType]] = Map()

  /**
    * Sets the numerical result corresponding to the method with the given name.
    *
    * @param name   The name of the method.
    * @param result The numerical result.
    */
  def setNumericalResult(name: String, result: CfgResult[NumericalStateType]): Unit =
    numerical += name -> result

  /**
    * Returns the numerical result corresponding to the method with the given
    * name.
    *
    * @param name The name of the method.
    * @return The numerical result.
    */
  def getNumericalResult(name: String = method.name): CfgResult[NumericalStateType] =
    numerical(name)

  /* ------------------------------------------------------------------------- *
   * Special Functions and Variables
   */

  private var readVariable: Option[sil.LocalVarDecl] = None

  private var maxFunction: Option[sil.Function] = None

  def getReadVariable: sil.LocalVarDecl = readVariable match {
    case Some(existing) => existing
    case None =>
      // create variable
      val name = uniqueIdentifier("read")
      val variable = sil.LocalVarDecl(name, sil.Perm)()
      // cache and return variable
      readVariable = Some(variable)
      variable
  }

  def getMaxFunction: sil.Function = maxFunction match {
    case Some(existing) => existing
    case None =>
      // create function
      val dx = sil.LocalVarDecl("x", sil.Perm)()
      val dy = sil.LocalVarDecl("y", sil.Perm)()
      val vx = sil.LocalVar("x")(sil.Perm)
      val vy = sil.LocalVar("y")(sil.Perm)
      val function = sil.Function(
        name = uniqueIdentifier("max"),
        formalArgs = Seq(dx, dy),
        typ = sil.Perm,
        pres = Seq(),
        posts = Seq(),
        decs = None,
        body = Some(sil.CondExp(sil.PermGtCmp(vx, vy)(), vx, vy)())
      )()
      // cache and return function
      maxFunction = Some(function)
      functions += ((function.name, function))
      function
  }

  /* ------------------------------------------------------------------------- *
   * code below has not been cleaned up
   */

  private var boundaryFunction: Option[sil.Function] = None

  private var quantifiedVariables: Map[sil.Type, Seq[sil.LocalVarDecl]] = Map()

  private var fieldAccessFunctions: Map[String, sil.Function] = Map()

  private var fieldAccessFunctionsInMethod: Map[String, Set[(String, sil.Function)]] = Map()

  private var sets: Map[(ProgramPoint, Expression), sil.LocalVarDecl] = Map()

  def getAuxiliaryFunctions: Map[String, Function] = functions

  def getFieldAccessFunction(field: String): sil.Function = {
    if (!fieldAccessFunctions.contains(field)) {
      val function = sil.Function(Context.createNewUniqueFunctionIdentifier("get" + field.head.toUpper + field.tail), Seq(sil.LocalVarDecl("x", sil.Ref)()), program.findField(field).typ, Seq(), Seq(), None, None)()
      fieldAccessFunctions += field -> function
      functions += function.name -> function
    }
    fieldAccessFunctionsInMethod += method.name -> (fieldAccessFunctionsInMethod.getOrElse(method.name, Set()) + ((field, fieldAccessFunctions(field))))
    fieldAccessFunctions(field)
  }

  def getPlaceholderFunction(quantifiedVariable: sil.LocalVarDecl): sil.Function = {
    val function = sil.Function(Context.createNewUniqueFunctionIdentifier("p"), Seq(quantifiedVariable), sil.Perm, Seq(), Seq(), None, None)()
    functions += function.name -> function
    function
  }


  def setMethodContext(program: sil.Program, method: SilverMethodDeclaration): Unit = {
    setProgram(program)
    selectMethod(method.name.name)
  }

  def prepareMethodForExtension(name: String): Unit = {
    selectMethod(name)
    identifiers --= sets.values.map(_.name)
    sets = Map()
  }

  def clearMethodSpecificInfo(): Unit = {
    readVariable match {
      case Some(varDecl) =>
        identifiers -= varDecl.name
        readVariable = None
      case None =>
    }
    identifiers --= quantifiedVariables.values.flatten.map(_.name)
    quantifiedVariables = Map()
    sets = Map()
  }

  private def typeToVarPrefix(typ: sil.Type): Char = typ match {
    case sil.Int => 'q'
    case _ => DefaultSilverConverter.convert(typ).name.toLowerCase()(0)
  }

  private def uniqueIdentifier(name: String, markAsTaken: Boolean = true): String = {
    var identifier = if (identifiers.contains(name)) {
      var count = 0
      while (identifiers.contains(name + count)) {
        count += 1
      }
      name + count
    } else name
    if (markAsTaken) identifiers += identifier
    identifier
  }

  def createNewUniqueVarIdentifier(name: String = "_var", markAsTaken: Boolean = true): String = uniqueIdentifier(name, markAsTaken)

  def createNewUniqueVarIdentifier(name: Char): String = uniqueIdentifier(name.toString)

  def createNewUniqueFunctionIdentifier(name: String = "_func"): String = uniqueIdentifier(name)

  def removeIdentifiers(identifiers: GenTraversableOnce[String]): Unit = this.identifiers --= identifiers

}
