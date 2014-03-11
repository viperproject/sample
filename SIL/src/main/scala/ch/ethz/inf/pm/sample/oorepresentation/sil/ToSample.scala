package ch.ethz.inf.pm.sample.oorepresentation.sil

import scala.collection.mutable
import semper.sil.{ast => sil}
import scala.Some
import semper.sil.ast.Predicate

trait SilConverter {
  /** Translates a whole SIL program to a list of Sample ClassDefinitions. */
  def convert(program: sil.Program): List[sample.ClassDefinition]

  /** Translates a SIL function to a Sample MethodDeclaration. */
  def convert(function: sil.Function): sample.MethodDeclaration

  /** Translates a SIL method to a Sample MethodDeclaration. */
  def convert(method: sil.Method): sample.MethodDeclaration

  /** Translates a SIL field to a Sample FieldDeclaration. */
  def convert(field: sil.Field): sample.FieldDeclaration

  /** Translates a SIL local variable to a Sample VariableDeclaration. */
  def convert(localVarDecl: sil.LocalVarDecl): sample.VariableDeclaration

  /** Translates a SIL node position to a Sample ProgramPoint. */
  def convert(pos: sil.Position): sample.ProgramPoint

  /** Translates a SIL type to a Sample type. */
  def convert(typ: sil.Type): sample.Type

  /** Translates a SIL statement to a Sample statement. */
  def convert(stmt: sil.Stmt): sample.Statement

  /** Translates a SIL expression to a Sample expression. */
  def convert(exp: sil.Exp): sample.Statement

  /** Translates a SIL predicate to a Sample predicate. */
  def convert(pred: sil.Predicate): Option[(sample.Identifier, sample.PredicateDefinition)]
}

object DefaultSilConverter extends SilConverter {
  var refType: sample.RefType = sample.RefType()
  var classDef: sample.ClassDefinition = null

  def convert(p: sil.Program): List[sample.ClassDefinition] = {
    // Chicken-egg problem: To build the reference type,
    // we need its list of fields and to build the list of fields
    // (possibly reference fields), we need the reference type.
    refType = sample.RefType()
    refType.fields = p.fields.map(makeVariableIdentifier).toSet

    classDef = new sample.ClassDefinition(
      programpoint = go(p.pos),
      typ = refType,
      modifiers = Nil,
      name = sample.ClassIdentifier(refType),
      parametricTypes = Nil,
      extend = Nil,
      fields = Nil,
      methods = Nil,
      pack = sample.PackageIdentifier,
      inv = sample.Constant("true", sample.BoolType, go(p.pos)))

    // Only translate the methods and fields once we have the ClassDefinition
    classDef.fields = p.fields.map(go).toList
    classDef.methods = p.functions.map(go).toList ++ p.methods.map(go).toList

    classDef :: Nil
  }

  def convert(f: sil.Function): sample.MethodDeclaration = {
    val resultVar = makeVariable(f.pos, f.typ, Constants.ResultVariableName)
    val resultVarDecl = sample.VariableDeclaration(go(f.pos), resultVar, go(f.typ))
    new MethodDeclWithOutParams(
      programpoint = go(f.pos),
      ownerType = refType,
      modifiers = sample.StaticModifier :: sample.PureModifier :: Nil,
      name = sample.MethodIdentifier(f.name),
      parametricType = Nil,
      arguments = f.formalArgs.map(go).toList :: List(resultVarDecl) :: Nil,
      returnType = go(f.exp.typ),
      body = {
        val cfg = new sample.ControlFlowGraph(go(f.pos))
        val resultAssign = sample.Assignment(go(f.pos), resultVar, right = go(f.exp))
        cfg.addNode(resultAssign :: resultVar :: Nil)
        cfg
      },
      precond = makeConjunction(f.pres),
      postcond = makeConjunction(f.posts),
      classDef
    )
  }

  def convert(m: sil.Method): sample.MethodDeclaration = {
    require(!m.body.existsDefined({ case g: sil.Goto => true }),
      "methods must not contain goto statements")

    new MethodDeclWithOutParams(
      programpoint = go(m.pos),
      ownerType = refType,
      modifiers = sample.StaticModifier :: Nil,
      name = sample.MethodIdentifier(m.name),
      parametricType = Nil,
      // SIL methods may return multiple values, so we model them as out-parameters.
      // The first list of parameters are in-parameters, the second list
      // are out-parameters (return values).
      arguments = m.formalArgs.map(go).toList :: m.formalReturns.map(go).toList :: Nil,
      // Method calls in SIL cannot occur in expressions, so it is fine to use
      // the return type 'null'. Depending on how the semantics is implemented,
      // it might also make sense not to use the return type for functions either.
      returnType = null,
      body = {
        val cfg = new sample.ControlFlowGraph(go(m.pos))
        // Put local variable declarations into a separate block
        val varBlock = cfg.addNode(m.locals.map(go).toList)
        cfg.addEdge(varBlock, convertCfg(m.body.toCfg)(cfg), None)
        cfg
      },
      precond = makeConjunction(m.pres),
      postcond = makeConjunction(m.posts),
      classDef = classDef)
  }

  def convert(f: sil.Field): sample.FieldDeclaration =
    new sample.FieldDeclaration(go(f.pos), modifiers = Nil,
      makeVariable(f), go(f.typ), right = None)

  def convert(v: sil.LocalVarDecl): sample.VariableDeclaration =
    new sample.VariableDeclaration(go(v.pos), makeVariable(v), go(v.typ))

  def convert(pos: sil.Position): sample.ProgramPoint = pos match {
    case sil.NoPosition => sample.DummyProgramPoint
    case pos: sil.RealPosition => sample.WrappedProgramPoint(pos)
  }

  def convert(p: sil.Type): sample.Type = p match {
    case sil.Bool => sample.BoolType
    case sil.Int => sample.IntType
    case sil.Ref => refType

    // Stubs
    case sil.Perm |
         sil.Pred |
         sil.DomainType(_, _) |
         sil.TypeVar(_) |
         sil.SeqType(_) |
         sil.SetType(_) |
         sil.MultisetType(_) =>
      sample.TopType
  }

  def convert(s: sil.Stmt): sample.Statement = s match {
    case sil.LocalVarAssign(lhs, rhs) =>
      sample.Assignment(go(s.pos), go(lhs), go(rhs))
    case sil.Assert(exp) =>
      makeNativeMethodCall(
        pos = s.pos,
        name = NativeMethods.assert.toString,
        args = exp :: Nil,
        returnType = sample.TopType)
    case sil.Inhale(exp) =>
      makeNativeMethodCall(
        pos = s.pos,
        name = NativeMethods.assume.toString,
        args = exp :: Nil,
        returnType = sample.TopType)
    case sil.NewStmt(lhs) =>
      sample.Assignment(go(s.pos), go(lhs), sample.New(go(s.pos), refType))
    case sil.FieldAssign(lhs, rhs) =>
      sample.Assignment(go(s.pos), go(lhs), go(rhs))
    case sil.MethodCall(method, args, targets) =>
      new sample.ContractAwareMethodCall(
        pp = go(s.pos),
        // Methods are static, so there is no receiver
        method = makeVariable(s.pos, sil.Ref, method.name),
        parametricTypes = Nil,
        parameters = args.map(go).toList,
        targets = targets.map(makeVariable).toList)

    // Stubs
    case sil.Exhale(e) =>
      sample.EmptyStatement(go(s.pos))
    case sil.Fold(acc) =>
      sample.EmptyStatement(go(s.pos))
    case sil.Unfold(acc) =>
      sample.EmptyStatement(go(s.pos))
    case sil.FreshReadPerm(_, _) | sil.Seqn(_) => ???
    case sil.Goto(_) |
         sil.If(_, _, _) |
         sil.Label(_) |
         sil.While(_, _, _, _) =>
      sys.error(s"unexpected statement $s (should not be part of the CFG)")
  }

  def convert(e: sil.Exp): sample.Statement = e match {
    case e: sil.DomainOpExp =>
      makeNativeMethodCall(e.pos, e.op, e.args, go(e.typ))
    case e: sil.EqualityCmp =>
      // No common ancestor
      makeNativeMethodCall(e.pos, e.op, e.args, go(e.typ))
    case l: sil.Literal =>
      sample.ConstantStatement(go(e.pos), l match {
        case sil.IntLit(i) => i.toString()
        case sil.BoolLit(value) =>
          value.toString // Use the strings "true" and "false"
        case sil.NullLit() => "null"
      }, go(l.typ))
    case v: sil.LocalVar => makeVariable(v)
    case sil.FuncLikeApp(func, args) =>
      new sample.ContractAwareFunctionCall(
        pp = go(e.pos),
        // Functions are static, so there is no receiver
        method = makeVariable(e.pos, sil.Ref, func.name),
        parametricTypes = Nil,
        parameters = args.map(go).toList,
        returnedType = go(e.typ))
    case sil.FieldAccess(rcv, field) =>
      sample.FieldAccess(go(e.pos), go(rcv), field.name, go(field.typ))
    case sil.CondExp(cond, thn, els) =>
      makeNativeMethodCall(
        pos = e.pos,
        name = NativeMethods.cond_exp.toString,
        args = cond :: thn :: els :: Nil,
        returnType = go(e.typ))
    case sil.Result() =>
      makeVariable(e.pos, e.typ, Constants.ResultVariableName)

    // Stubs
    case sil.AccessPredicate(loc, perm) =>
      go(sil.TrueLit()()) // May not be what we want
    case sil.QuantifiedExp(vars, inner) =>
      go(sil.TrueLit()()) // May not be what we want
    case sil.Unfolding(acc, inner) =>
      go(inner)
    case sil.InhaleExhaleExp(in, ex) =>
      go(sil.TrueLit()()) // May not be what we want
    case p: sil.PermExp => ???
    case sil.Old(exp) =>
      // TODO: Is only sound for local variables
      go(exp)
    case e: sil.SeqExp => ???
    case sil.PredicateAccess(args, pred) => ???
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

  def convert(pred: Predicate): Option[(sample.Identifier, sample.PredicateDefinition)] = {
    if (pred.formalArgs.map(_.typ) == Seq(sil.Ref)) {
      // Only support SIL predicates with a single reference parameter
      val formalArg = pred.formalArgs.head.localVar
      val nullLit = sil.NullLit()()

      val samplePredId = sample.VariableIdentifier(pred.name)(PredType)

      var isSupported = true
      val fieldsWithPerm = pred.body.reduceTree[Map[sample.Identifier, sample.NestedPredDefDomain]]({
        case (sil.And, res) =>
          res.flatMap(map => { map }).toMap
        case (sil.FieldAccessPredicate(sil.FieldAccess(rcv, field), sil.FullPerm), res)
          if formalArg == rcv =>
          // Found a field access predicate for a field of the formal argument
          Map(makeVariableIdentifier(field) -> sample.NestedPredDefDomain())
        case (sil.Implies(
          sil.NeCmp(leftCmp_, rightCmp_),
          sil.PredicateAccessPredicate(sil.PredicateAccess(args, nestedPred), sil.FullPerm))) =>

          // The null literal could be on both sides of the inequality: Sort
          val (leftCmp, rightCmp) =
            if (leftCmp_ == nullLit) (rightCmp_, leftCmp_)
            else (leftCmp_, rightCmp_)

          (leftCmp, rightCmp) match {
            case (fa @ sil.FieldAccess(rcv, field), sil.NullLit)
              if formalArg == rcv && args == Seq(fa) && nestedPred == pred =>
                Map(makeVariableIdentifier(field) -> sample.NestedPredDefDomain().add(samplePredId))
            case _ =>
              isSupported = false
              Map.empty
          }
        case _ =>
          // If the predicate contains anything else, give up
          isSupported = false
          Map.empty
      })

      if (isSupported) {
        val samplePredDef = sample.PredicateDefinition(fieldsWithPerm)
        Some(samplePredId -> samplePredDef)
      } else None
    }
    else None
  }

  /**
   * Translates a SIL CFG block, adds it to the given Sample CFG and recurses to its successors.
   * @param b the SIL block
   * @param cfg the Sample CFG to extend
   * @param indices maps already translated SIL blocks to Sample CFG node indices
   * @return the Sample CFG node index corresponding to the SIL block
   */
  private def convertCfg(b: sil.Block)(
    implicit cfg: sample.ControlFlowGraph,
    indices: mutable.Map[sil.Block, Int] = mutable.Map.empty): Int = {
    if (indices.contains(b)) indices(b)
    else {
      val stmts = b match {
        case b: sil.ConditionalBlock =>
          b.stmt.children.map(go).toList ++ (go(b.cond) :: Nil)
        case b: sil.StatementBlock =>
          b.stmt.children.map(go).toList
        case lb: sil.LoopBlock =>
          // Generate an assertion for each loop invariant
          val assertMethodCalls = lb.invs.toList.map(inv => {
            go(sil.Assert(inv)(inv.pos))
          })
          assertMethodCalls :+ go(lb.cond)
        case b: sil.FreshReadPermBlock =>
          sample.EmptyStatement(sample.DummyProgramPoint) :: Nil
      }
      // Create new node and register its index *before* recursing
      val index = cfg.addNode(stmts)
      indices.update(b, index)
      b match {
        case b: sil.ConditionalBlock =>
          cfg.addEdge(index, convertCfg(b.thn), Some(true))
          cfg.addEdge(index, convertCfg(b.els), Some(false))
        case b: sil.LoopBlock =>
          cfg.addEdge(index, convertCfg(b.body), Some(true))
          // In the SIL CFG, there are no explicit edges from blocks at
          // the end of the loop back to the containing loop block.
          // For Sample, we need to add them explicitly.
          // Thus, find all terminal blocks reachable from the loop body block
          // and add a back-edge.
          val terminalBodyBlocks = sil.utility.ControlFlowGraph
            .collectBlocks(b.body).filter(_.isInstanceOf[sil.TerminalBlock])

          for (terminalBlock <- terminalBodyBlocks) {
            cfg.addEdge(convertCfg(terminalBlock), index, None)
          }

          cfg.addEdge(index, convertCfg(b.succ), Some(false))
        case _ =>
          // TODO: Should handle FreshReadPermBlock
          for (succ <- b.succs)
            cfg.addEdge(index, convertCfg(succ.dest), None)
      }
      index
    }
  }

  /**
   * Translates a conjunction of boolean SIL expressions to a Statement.
   * @param conj sequence of boolean SIL expressions
   * @return true if the list of SIL expressions is empty
   */
  private def makeConjunction(conj: Seq[sil.Exp]): sample.Statement = go(conj match {
    case Nil => sil.TrueLit()()
    case _ => conj.reduceRight((x, y) => new sil.And(x, y)(x.pos))
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
      pos: sil.Position,
      name: String,
      args: Seq[sil.Exp],
      returnType: sample.Type): sample.Statement = {
    sample.MethodCall(go(pos),
      method = sample.FieldAccess(go(pos), go(args.head), name, null),
      parametricTypes = Nil,
      parameters = args.tail.map(go).toList,
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

  private def go(p: sil.Predicate) = convert(p)
}