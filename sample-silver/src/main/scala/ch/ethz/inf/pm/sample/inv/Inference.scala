/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.inv

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.IntegerOctagons
import ch.ethz.inf.pm.sample.analysis.{HeapAndOctagonAnalysisEntryState, SimpleHeapAndSemanticAnalysisState}
import ch.ethz.inf.pm.sample.domain.{HeapNode, MayAliasGraph}
import ch.ethz.inf.pm.sample.execution.{CfgResult, SimpleSilverForwardAnalysis}
import ch.ethz.inf.pm.sample.inference.{SilverExtender, SilverInferenceRunner}
import ch.ethz.inf.pm.sample.oorepresentation.silver.DefaultSampleConverter
import ch.ethz.inf.pm.sample.util.Verifiers
import viper.silver.{ast => sil}

trait Inference {
  /**
    * Extends the given program.
    *
    * @param program The program to extend.
    * @return An optional holding the extended program or an empty optional.
    */
  def extend(program: sil.Program): Option[sil.Program] = {
    val extendedMethods = program.methods.map { method =>
      extendMethod(method, program) match {
        case Some(extended) => extended
        case None => method
      }
    }
    val extendedProgram = program.copy(methods = extendedMethods)(program.pos, program.info, program.errT)
    Some(extendedProgram)
  }

  def extendMethod(method: sil.Method, program: sil.Program): Option[sil.Method]
}

/**
  * An inference that infers invariants by enumerating hypotheses until a hypothesis is found that makes the program
  * verify.
  */
trait InvariantInference extends Inference {
  /**
    * Returns the hypothesis enumerator.
    *
    * @param context An object holding some context information such as the program being analyzed, locally defined
    *                variables etc.
    * @return The hypothesis enumerator.
    */
  protected def hypotheses(context: Context, existing: Seq[sil.Exp]): Enumerator[Seq[sil.Exp]]

  override def extendMethod(method: sil.Method, program: sil.Program): Option[sil.Method] =
    method.body
      .flatMap { body =>
        val context = Context(program)
        val variables = method.formalArgs.map { argument => sil.LocalVar(argument.name)(argument.typ) }
        val updated = context.addVariables(variables).setPreconditions(method.pres).setPostconditions(method.posts)
        extendBody(body, updated)
      }
      .map { body => method.copy(body = Some(body))(method.pos, method.info, method.errT) }

  def extendBody(body: sil.Seqn, context: Context): Option[sil.Seqn] = extendStatement(body, context) match {
    case Some(sequence: sil.Seqn) => Some(sequence)
    case _ => None
  }

  def extendStatement(statement: sil.Stmt, context: Context): Option[sil.Stmt] = statement match {
    // extend sequence
    case sil.Seqn(statements, declarations) =>
      // get variables
      val variables = declarations
        .collect { case declaration: sil.LocalVarDecl => declaration }
        .map { declaration => sil.LocalVar(declaration.name)(declaration.typ) }
      // split list of statements into parts containing no loops
      val parts = statements.foldRight(List.empty[sil.Stmt] :: Nil) {
        case (statement, last :: list) =>
          // check whether the statement contains a loop
          val hasLoop = statement.exists {
            case _: sil.While => true
            case _ => false
          }
          // update parts
          if (hasLoop) List.empty[sil.Stmt] :: List(statement) :: last :: list
          else (last :+ statement) :: list
      }
      // how many loops are there?
      parts match {
        // there is no loop
        case all :: Nil =>
          val sequence = sil.Seqn(all, declarations)()
          Some(sequence)
        // there is exactly one statement that contains a loop
        case before :: (statement :: Nil) :: after :: Nil =>
          val updated = context.addVariables(variables).addBefore(before).addAfter(after)
          extendStatement(statement, updated) match {
            case Some(extended) =>
              val sequence = sil.Seqn((before :+ extended) ++ after, declarations)()
              Some(sequence)
            case _ => None
          }
      }
    // extend if statement
    case sil.If(condition, body1, body2) =>
      // extend body of if branch
      val extended1 = {
        val assumption = Seq(sil.Inhale(condition)())
        val updated = context.addBefore(assumption)
        extendBody(body1, updated)
      }
      // extend body of else branch
      val extended2 = {
        val assumption = Seq(sil.Inhale(sil.Not(condition)())())
        val updated = context.addBefore(assumption)
        extendBody(body2, updated)
      }
      // stitch together extended bodies
      (extended1, extended2) match {
        case (Some(first), Some(second)) =>
          val conditional = sil.If(condition, first, second)()
          Some(conditional)
        case _ => None
      }
    // extend while loop
    case loop: sil.While =>
      extendLoop(loop, context)
    case _ => ???
  }

  def extendLoop(loop: sil.While, context: Context): Option[sil.While] = {
    // get loop condition and body
    val body = loop.body
    val condition = loop.cond
    // iterate over hypotheses in order to search for a valid one
    hypotheses(context, loop.invs)
      .map { hypothesis =>
        println(hypothesis)
        val invariants = loop.invs ++ hypothesis
        val innerContext = context
          .setBefore(Seq.empty)
          .setAfter(Seq.empty)
          .setPreconditions(invariants :+ condition)
          .setPostconditions(invariants.reverse)
        extendBody(body, innerContext) match {
          case Some(extendedBody) =>
            lazy val inner = check(extendedBody :: Nil, innerContext)
            lazy val outer = {
              val negation = sil.Not(loop.cond)()
              val exhales = invariants.reverse.map { condition => sil.Exhale(condition)() }
              val inhales = (invariants :+ negation).map { condition => sil.Inhale(condition)() }
              check(exhales ++ inhales, context)
            }
            val extended = sil.While(condition, invariants, extendedBody)()
            (inner && outer, extended)
          case _ => (false, loop)
        }
      }
      .find { case (valid, _) => valid }
      .map { case (_, extended) => extended }
      .getOption
  }

  def check(statements: Seq[sil.Stmt], context: Context): Boolean = {
    // build method
    val method = {
      val body = {
        val variables = context.variables.map { variable => sil.LocalVarDecl(variable.name, variable.typ)() }
        val preconditions = context.preconditions.map { condition => sil.Inhale(condition)() }
        val postconditions = context.postconditions.map { condition => sil.Exhale(condition)() }
        val sequence = sil.Seqn(preconditions ++ context.before ++ statements ++ context.after ++ postconditions, variables)()
        Some(sequence)
      }
      sil.Method("main", Seq.empty, Seq.empty, Seq.empty, Seq.empty, body)()
    }
    // build and verify program
    val program = {
      val original = context.program
      val methods = Seq(method)
      original.copy(methods = methods)(original.pos, original.info, original.errT)
    }
    Verifiers.doesVerify(program)
  }

  /**
    * Represents some context information.
    *
    * @param program        The program being analyzed.
    * @param variables      The variables in the current scope.
    * @param preconditions  The preconditions.
    * @param postconditions The postconditions.
    * @param before         The sequence of statements before.
    * @param after          The sequence of statements after.
    */
  protected case class Context(program: sil.Program,
                               variables: Seq[sil.LocalVar] = Seq.empty,
                               preconditions: Seq[sil.Exp] = Seq.empty,
                               postconditions: Seq[sil.Exp] = Seq.empty,
                               before: Seq[sil.Stmt] = Seq.empty,
                               after: Seq[sil.Stmt] = Seq.empty) {

    /**
      * Sets the variables of the context.
      *
      * @param variables The variables to be set.
      * @return The updated context.
      */
    def setVariables(variables: Seq[sil.LocalVar]): Context = copy(variables = variables)

    /**
      * Sets the preconditions of the context.
      *
      * @param conditions The preconditions to be set.
      * @return The updated context.
      */
    def setPreconditions(conditions: Seq[sil.Exp]): Context = copy(preconditions = conditions)

    /**
      * Sets the postconditions of the context.
      *
      * @param conditions The postconditions to be set.
      * @return The updated context.
      */
    def setPostconditions(conditions: Seq[sil.Exp]): Context = copy(postconditions = conditions)

    /**
      * Sets the sequence of statements before.
      *
      * @param statements The statements to be set.
      * @return The updated context.
      */
    def setBefore(statements: Seq[sil.Stmt]): Context = copy(before = statements)

    /**
      * Sets the sequence of statements after.
      *
      * @param statements The statements to be set.
      * @return The updated context.
      */
    def setAfter(statements: Seq[sil.Stmt]): Context = copy(after = statements)

    /**
      * Adds the given variables to the variables of the context.
      *
      * @param variables The variables to be added.
      * @return The updated context.
      */
    def addVariables(variables: Seq[sil.LocalVar]): Context = setVariables(this.variables ++ variables)

    /**
      * Adds the given conditions to the preconditions of the context.
      *
      * @param conditions The conditions to be added.
      * @return The updated context.
      */
    def addPreconditions(conditions: Seq[sil.Exp]): Context = setPreconditions(preconditions ++ conditions)

    /**
      * Adds the given conditions to the postconditions of the context.
      *
      * @param conditions The conditions to be added.
      * @return The updated context.
      */
    def addPostconditions(conditions: Seq[sil.Exp]): Context = setPostconditions(postconditions ++ conditions)

    /**
      * Appends the given statements to the statements before.
      *
      * @param statements The statements to be appended.
      * @return The updated context.
      */
    def addBefore(statements: Seq[sil.Stmt]): Context = setBefore(before ++ statements)

    /**
      * Prepends the given statements to the statements after.
      *
      * @param statements The statements to be prepended.
      * @return The updated context.
      */
    def addAfter(statements: Seq[sil.Stmt]): Context = setAfter(statements ++ after)
  }

}

object PermissionInvariantInference extends InvariantInference {

  override def hypotheses(context: Context, existing: Seq[sil.Exp]): Enumerator[Seq[sil.Exp]] = {
    val references = context.variables.filter { variable => variable.typ == sil.Ref }
    val variables = Enumerator.sequence[sil.Exp](references)
    val fields = Enumerator.sequence(context.program.fields)
    val simple = accesses(variables, fields)
    Enumerator.tuples(simple)
  }

  private def accesses(receivers: Enumerator[sil.Exp], fields: Enumerator[sil.Field]): Enumerator[sil.Exp] =
    Enumerator.product(receivers, fields, (receiver: sil.Exp, field: sil.Field) => {
      val location = sil.FieldAccess(receiver, field)()
      val permission = sil.FullPerm()()
      sil.FieldAccessPredicate(location, permission)()
    })
}

object NumericalInvariantInference extends InvariantInference {

  private val UPPER = 2

  override protected def hypotheses(context: NumericalInvariantInference.Context, existing: Seq[sil.Exp]): Enumerator[Seq[sil.Exp]] = {
    // get numerical variables
    val numerical = context.variables.filter { variable => variable.typ == sil.Int }
    // get fields to which we have access
    val fields = existing.flatMap {
      case sil.FieldAccessPredicate(location, _) if location.typ == sil.Int => Some(location: sil.Exp)
      case _ => None
    }

    val locations = Enumerator.sequence(numerical ++ fields)
    val signed = locations.flatMap { location => Enumerator.sequence(Seq(location, sil.Minus(location)())) }

    val constants = Enumerator.sequence(0 to 2 * UPPER).map { integer =>
      val absolute = (integer + 1) / 2
      val sign = (integer % 2) * 2 - 1
      val constant = absolute * sign
      sil.IntLit(constant)()
    }

    val singles = Enumerator.product(signed, constants, (expression: sil.Exp, bound: sil.Exp) => sil.LeCmp(expression, bound)(): sil.Exp)
    val pairs = Enumerator.product(signed, signed, (left: sil.Exp, right: sil.Exp) => sil.Add(left, right)())
    val doubles = Enumerator.product(pairs, constants, (expression: sil.Exp, bound: sil.Exp) => sil.LeCmp(expression, bound)(): sil.Exp)

    val atoms = Enumerator.joined(singles, doubles)
    Enumerator.tuples(atoms)
  }

}

object NumericalInference extends Inference {

  type Heap = MayAliasGraph

  type Semantic = IntegerOctagons

  type State = SimpleHeapAndSemanticAnalysisState[Heap, Semantic, HeapNode]

  val numerical = new SilverInferenceRunner[State] with SilverExtender[State] {

    override val analysis = SimpleSilverForwardAnalysis(HeapAndOctagonAnalysisEntryState)

    override def inferInvariants(loop: sil.While, result: CfgResult[State]): Seq[sil.Exp] = {
      val existing = loop.invs
      val position = getLoopPosition(loop, result.cfg)
      val inferred = result.preStateAt(position).specifications
      val converted = inferred.map(DefaultSampleConverter.convert).toSeq
      existing ++ converted
    }
  }

  override def extendMethod(method: sil.Method, program: sil.Program): Option[sil.Method] = {
    val updated = program.copy(methods = Seq(method))(program.pos, program.info, program.errT)
    val extended = numerical.extend(updated).methods.head
    Some(extended)
  }
}

object ExtendedInference extends Inference {

  override def extendMethod(method: sil.Method, program: sil.Program): Option[sil.Method] = {

    val updated = method.copy(pres = permissions(method.pres), posts = permissions(method.posts))(method.pos, method.info, method.errT)
    val extended = PermissionInvariantInference.extendMethod(updated, program)

    extended.foreach { m => println("permission:\n" + m) }

    extended.flatMap { extended =>
      val updated = extended.copy(pres = method.pres, posts = method.posts)(method.pos, method.info, method.errT)
      val result = NumericalInvariantInference.extendMethod(updated, program)
      result.foreach { m => println("permission:\n" + m) }
      result
    }

  }

  private def permissions(conditions: Seq[sil.Exp]): Seq[sil.Exp] =
    conditions.foldLeft(Seq.empty[sil.Exp]) {
      case (result, condition) =>
        result ++ permissions(condition)
    }

  private def permissions(condition: sil.Exp): Seq[sil.Exp] = condition match {
    case sil.And(left, right) => permissions(left) ++ permissions(right)
    case predicate: sil.FieldAccessPredicate => Seq(predicate)
    case _ => Seq.empty
  }
}
