/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.inv

import ch.ethz.inf.pm.sample.util.Verifiers
import viper.silver.ast.Program
import viper.silver.{ast => sil}

trait Inference {
  /**
    * Extends the given program.
    *
    * @param program The program to extend.
    * @return An optional holding the extended program or an empty optional.
    */
  def extend(program: sil.Program): Option[sil.Program]
}

object SimpleInference extends Inference {

  override def extend(program: Program): Option[sil.Program] = {

    val context = Context(program)

    val extendedMethods = program.methods.map { method =>
      val variables = method.formalArgs.map { argument => sil.LocalVar(argument.name)(argument.typ) }
      val updated = context.addVariables(variables).setPreconditions(method.pres).setPostconditions(method.posts)
      val extendedBody = method.body.flatMap { body => extendBody(body, updated) }
      method.copy(body = extendedBody)(method.pos, method.info, method.errT)
    }
    val extendedProgram = program.copy(methods = extendedMethods)(program.pos, program.info, program.errT)
    Some(extendedProgram)
  }

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
    hypotheses(context)
      .map { hypothesis =>
        val invariants = hypothesis ++ loop.invs
        val innerContext = context
          .setBefore(Seq.empty)
          .setAfter(Seq.empty)
          .setPreconditions(invariants :+ condition)
          .setPostconditions(invariants)
        extendBody(body, innerContext) match {
          case Some(extendedBody) =>
            lazy val inner = check(extendedBody :: Nil, innerContext)
            lazy val outer = {
              val negation = sil.Not(loop.cond)()
              val exhales = invariants.map { condition => sil.Exhale(condition)() }
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

  private def hypotheses(context: Context): Enumerator[Seq[sil.Exp]] = {
    val variables = Enumerator.sequence[sil.Exp](context.variables)
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
  case class Context(program: sil.Program,
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
