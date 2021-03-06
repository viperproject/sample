/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.oorepresentation.silver

import ch.ethz.inf.pm.sample.execution.SampleCfg
import ch.ethz.inf.pm.sample.oorepresentation.{Statement, TaggedProgramPoint}
import com.typesafe.scalalogging.LazyLogging
import viper.silver.{ast => sil}

import scala.language.reflectiveCalls

/** Object enumerating methods to handle permissions and specifications (i.e.
  * preconditions, postconditions and invariants).
  *
  * @author Caterina Urban
  * @author Jerome Dohrau
  */
object SilverMethods extends Enumeration {
  val access = Value(Constants.GhostSymbolPrefix + "access")
  val permission = Value(Constants.GhostSymbolPrefix + "permission")
  val inhale = Value(Constants.GhostSymbolPrefix + "inhale")
  val exhale = Value(Constants.GhostSymbolPrefix + "exhale")
}

trait SilverConverter {
  /**
    * Converts a whole Silver program to a list of Sample class definition.
    */
  def convert(program: sil.Program): SilverProgramDeclaration

  /**
    * Converts a SIL function to a Sample method declaration.
    */
  def convert(function: sil.Function): SilverFunctionDeclaration

  /**
    * Converts a SIL method to a Sample method declaration.
    */
  def convert(method: sil.Method): SilverMethodDeclaration

  /**
    * Converts a SIL field to a Sample field declaration.
    */
  def convert(field: sil.Field): sample.FieldDeclaration

  /**
    * Converts a SIL local variable to a Sample variable declaration.
    */
  def convert(localVarDecl: sil.LocalVarDecl): sample.VariableDeclaration

  /**
    * Converts a SIL node position to a Sample ProgramPoint.
    */
  def convert(pos: sil.Position): sample.ProgramPoint

  /**
    * Converts a SIL type to a Sample type.
    */
  def convert(typ: sil.Type): sample.Type

  /**
    * Converts a SIL statement to a Sample statement.
    *
    * The method returns a list of sequence since there are cases where one
    * statements gets translated to several statements.
    */
  def convert(stmt: sil.Stmt): Seq[sample.Statement]

  /**
    * Converts a SIL expression to a Sample expression.
    */
  def convert(exp: sil.Exp): sample.Statement
}

object DefaultSilverConverter extends SilverConverter with LazyLogging {
  var refType: sample.RefType = sample.RefType()
  var prog: sil.Program = _

  def convert(p: sil.Program): SilverProgramDeclaration = {
    // Chicken-egg problem: To build the reference type,
    // we need its list of fields and to build the list of fields
    // (possibly reference fields), we need the reference type.
    refType = sample.RefType()
    refType.fields = p.fields.map(makeVariableIdentifier).toSet
    prog = p

    val program = new SilverProgramDeclaration(
      fields = p.fields.map(go).toList,
      functions = p.functions.map(go).toList,
      methods = p.methods.map(go).toList
    )

    program
  }

  def convert(f: sil.Function): SilverFunctionDeclaration = {
    new SilverFunctionDeclaration(
      programPoint = go(f.pos),
      name = SilverIdentifier(f.name),
      parameters = f.formalArgs.map(go).toList,
      returnType = go(f.typ),
      body = f.body match {
        case Some(b) => Option(go(b))
        case None => None
      }
    )
  }

  def convert(method: sil.Method): SilverMethodDeclaration =
    new SilverMethodDeclaration(
      programPoint = go(method.pos),
      name = SilverIdentifier(method.name),
      arguments = method.formalArgs.map(go).toList,
      returns = method.formalReturns.map(go).toList,
      returnType = method.formalReturns.map(_.typ).map(go).toList,
      body = method.toCfg().map[SampleCfg, Statement, Statement](SampleCfg())(go, go)
    )

  def convert(f: sil.Field): sample.FieldDeclaration =
    new sample.FieldDeclaration(go(f.pos), modifiers = Nil,
      makeVariable(f), go(f.typ), right = None)

  def convert(v: sil.LocalVarDecl): sample.VariableDeclaration =
    new sample.VariableDeclaration(go(v.pos), makeVariable(v), go(v.typ))

  def convert(pos: sil.Position): sample.ProgramPoint = pos match {
    case sil.NoPosition => sample.DummyProgramPoint
    case pos: sil.HasLineColumn => sample.WrappedProgramPoint(pos)
  }

  def convert(p: sil.Type): sample.Type = p match {
    case sil.Bool => sample.BoolType
    case sil.Int => sample.IntType
    case sil.Ref => refType
    case sil.Perm => sample.PermType
    case sil.DomainType(name, _) => sample.DomType(name)

    // Stubs
    case sil.TypeVar(_) |
         sil.SeqType(_) |
         sil.SetType(_) |
         sil.MultisetType(_) =>
      sample.TopType
  }


  def convert(s: sil.Stmt): Seq[sample.Statement] = s match {
    case sil.LocalVarAssign(lhs, rhs) =>
      val assignment = sample.Assignment(go(s.pos), go(lhs), go(rhs))
      Seq(assignment)

    case sil.Assert(exp) =>
      val assert = makeNativeMethodCall(
        pos = go(s.pos),

        name = NativeMethods.assert.toString,
        args = go(exp) :: Nil,
        returnType = sample.TopType)
      Seq(assert)

    case sil.NewStmt(lhs, fields) =>
      val receiver = go(lhs)
      val statement: sample.Statement = sample.Assignment(go(s.pos), receiver, sample.New(go(s.pos), refType))
      fields.foldLeft(Seq(statement)) { (statements, field) =>
        // create inhale for field
        val location = sil.FieldAccess(lhs, field)()
        val predicate = sil.FieldAccessPredicate(location, sil.FullPerm()())()
        val inhale = makeNativeMethodCall(
          pos = TaggedProgramPoint(go(s.pos), field.name),
          name = SilverMethods.inhale.toString,
          args = go(predicate) :: Nil,
          returnType = sample.TopType)
        // add inhale to list of statements
        statements ++ Seq(inhale)
      }

    case sil.FieldAssign(lhs, rhs) =>
      val assignment = sample.Assignment(go(s.pos), go(lhs), go(rhs))
      Seq(assignment)

    case sil.MethodCall(method, args, targets) => {
      val call = sample.MethodCall(
        pp = go(s.pos),
        method = makeVariable(s.pos, sil.Ref, method),
        parametricTypes = Nil,
        parameters = args.map(go).toList,
        returnedType = sample.TopType,
        targets = targets.map(go).toList)
      Seq(call)
    }

    case sil.LocalVarDeclStmt(decl) =>
      Seq(go(decl))

    // Inhale and Exhale statements are converted into native method calls
    // @author Caterina Urban
    case sil.Inhale(exp) =>
      val inhale = makeNativeMethodCall(
        pos = go(s.pos),
        name = SilverMethods.inhale.toString,
        args = go(exp) :: Nil,
        returnType = sample.TopType)
      Seq(inhale)

    case sil.Exhale(exp) =>
      val exhale = makeNativeMethodCall(
        pos = go(s.pos),
        name = SilverMethods.exhale.toString,
        args = go(exp) :: Nil,
        returnType = sample.TopType)
      Seq(exhale)

    // Stubs
    case sil.Fold(acc) =>
      val empty = sample.EmptyStatement(go(s.pos))
      Seq(empty)

    case sil.Unfold(acc) =>
      val empty = sample.EmptyStatement(go(s.pos))
      Seq(empty)

    case sil.Seqn(_, _) =>
      ???

    case sil.Goto(_) |
         sil.If(_, _, _) |
         sil.Label(_, _) |
         sil.While(_, _, _) =>
      sys.error(s"unexpected statement $s (should not be part of the CFG)")
  }

  def convert(e: sil.Exp): sample.Statement = e match {
    case e: sil.DomainOpExp =>
      makeNativeMethodCall(go(e.pos), e.func(prog).op, e.args.map(go), go(e.typ))
    case e: sil.EqualityCmp =>
      // No common ancestor
      makeNativeMethodCall(go(e.pos), e.op, e.args.map(go), go(e.typ))
    case l: sil.Literal =>
      sample.ConstantStatement(go(e.pos), l match {
        case sil.IntLit(i) => i.toString()
        case sil.BoolLit(value) => value.toString // use the strings "true" and "false"
        case sil.NullLit() => "null"
      }, go(l.typ))
    case v: sil.LocalVar => makeVariable(v)
    case sil.FuncLikeApp(func, args) =>
      sample.MethodCall(
        pp = go(e.pos),
        // Functions are static, so there is no receiver
        method = makeVariable(e.pos, sil.Ref, func),
        parametricTypes = Nil,
        parameters = args.map(go).toList,
        returnedType = go(e.typ))
    case sil.FieldAccess(rcv, field) =>
      sample.FieldAccess(go(e.pos), go(rcv), field.name, go(field.typ))
    case sil.CondExp(cond, thn, els) =>
      makeNativeMethodCall(
        pos = go(e.pos),
        name = NativeMethods.cond_exp.toString,
        args = go(cond) :: go(thn) :: go(els) :: Nil,
        returnType = go(e.typ))
    case _: sil.Result =>
      makeVariable(e.pos, e.typ, Constants.ResultVariableName)
    case sil.Unfolding(acc, inner) =>
      go(inner)

    // access predicates
    case sil.FieldAccessPredicate(location, permission) => makeNativeMethodCall(
      pos = go(e.pos),
      name = SilverMethods.access.toString,
      args = go(location) :: go(permission) :: Nil,
      returnType = sample.PermType
    )

    // permission expressions
    case sil.FullPerm() => go(sil.FractionalPerm(sil.IntLit(1)(), sil.IntLit(1)())())
    case sil.NoPerm() => go(sil.FractionalPerm(sil.IntLit(0)(), sil.IntLit(1)())())
    case sil.FractionalPerm(left, right) => makeNativeMethodCall(
      pos = go(e.pos),
      name = ???,
      args = go(left) :: go(right) :: Nil,
      returnType = sample.PermType
    )

    case sil.CurrentPerm(loc: sil.Exp) => makeNativeMethodCall(
      pos = go(e.pos),
      name = SilverMethods.permission.toString,
      args = go(loc) :: Nil,
      returnType = sample.PermType)

    // SeqExp (e.g., data : Seq[Int]) are smashed into summary variables (e.g., data : Int)
    // their length (e.g., |data|) is treated as another unbounded variable
    // @author Caterina Urban

    case e: sil.SeqLength => // sequence length, e.g., |this.data|
      makeVariable(e.pos, e.typ, e.toString)
    case sil.SeqIndex(sequence, index) =>
      sample.FieldAccess(go(e.pos), go(sequence), "[]", go(e.typ))
    case e: sil.SeqExp => throw new NotImplementedError("A sil.SeqExp conversion is missing!")

    // Stubs
    case sil.QuantifiedExp(vars, inner) =>
      go(sil.TrueLit()()) // Ignore
    case sil.InhaleExhaleExp(in, ex) =>
      go(sil.TrueLit()()) // Ignore

    case sil.Old(exp) => ???
    case sil.AnySetCardinality(_) |
         sil.AnySetContains(_, _) |
         sil.AnySetIntersection(_, _) |
         sil.AnySetMinus(_, _) |
         sil.AnySetSubset(_, _) |
         sil.AnySetUnion(_, _) |
         sil.EmptyMultiset(_) |
         sil.EmptySet(_) |
         sil.ExplicitMultiset(_) |
         sil.ExplicitSet(_) => ???
  }

  /** Flattens a SIL conjunction into a sequence of expressions.
    *
    * For example, given the expression And(And(a, b), c), the result is
    * Seq(a, b, c). If the root of expression is not a conjunction,
    * the method just returns the expression itself.
    */
  private def flattenConjunction(exp: sil.Exp): Seq[sil.Exp] = exp match {
    case sil.And(left, right) =>
      flattenConjunction(left) ++ flattenConjunction(right)
    case _ =>
      Seq(exp)
  }

  /**
    * Converts a conjunction of boolean SIL expressions to a Statement.
    *
    * @param conj sequence of boolean SIL expressions
    * @return true if the list of SIL expressions is empty
    */
  private def makeConjunction(conj: Seq[sil.Exp]): sample.Statement = go(conj match {
    case Nil => sil.TrueLit()()
    case _ => conj.reduceRight((x, y) => sil.And(x, y)(x.pos))
  })

  /**
    * Creates a new Sample variable.
    * Use structural subtyping for the parameter because `Field` and
    * `LocalVarDecl` declare the `name` field independently.
    * Alternatively, one could add a trait `Named` to them.
    */
  private type Named = {def name: String}
  private type LocalVarOrField = sil.Node with sil.Typed with sil.Positioned with Named

  private def makeVariable(n: LocalVarOrField) =
    sample.Variable(go(n.pos), makeVariableIdentifier(n))

  private def makeVariableIdentifier(n: LocalVarOrField) =
    sample.VariableIdentifier(n.name)(go(n.typ), go(n.pos))

  private def makeVariable(pos: sil.Position, typ: sil.Type, name: String) =
    sample.Variable(go(pos), sample.VariableIdentifier(name)(go(typ), go(pos)))

  private def makeNativeMethodCall(
                                    pos: sample.ProgramPoint,
                                    name: String,
                                    args: Seq[sample.Statement],
                                    returnType: sample.Type): sample.Statement = {
    sample.MethodCall(pos,
      method = sample.FieldAccess(pos, args.head, name, null),
      parametricTypes = Nil,
      parameters = args.tail.toList,
      returnType)
  }

  // Convenience aliases
  private def go(f: sil.Function) = convert(f)

  private def go(m: sil.Method) = convert(m)

  private def go(f: sil.Field) = convert(f)

  private def go(v: sil.LocalVarDecl) = convert(v)

  private def go(pos: sil.Position) = convert(pos)

  private def go(t: sil.Type) = convert(t)

  private def go(s: sil.Stmt) = convert(s)

  private def go(e: sil.Exp) = convert(e)
}