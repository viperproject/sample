/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.oorepresentation.silver

import ch.ethz.inf.pm.sample.oorepresentation.DummyProgramPoint
import com.typesafe.scalalogging.LazyLogging
import viper.silver.{ast => sil}

import scala.language.reflectiveCalls
import scala.collection.mutable

/** Object enumerating methods to handle permissions and specifications (i.e.
  * preconditions, postconditions and invariants).
  *
  * @author Caterina Urban
  * @author Jerome Dohrau
  */
object SilverMethods extends Enumeration {
  val permission = Value(Constants.GhostSymbolPrefix + "permission")
  val inhale = Value(Constants.GhostSymbolPrefix + "inhale")
  val exhale = Value(Constants.GhostSymbolPrefix + "exhale")
  val precondition = Value(Constants.GhostSymbolPrefix + "precondition")
  val postcondition = Value(Constants.GhostSymbolPrefix + "postcondition")
  val invariant = Value(Constants.GhostSymbolPrefix + "invariant")
}

trait SilverConverter {
  /**
    * Converts a whole Silver program to a list of Sample class definition.
    */
  def convert(program: sil.Program): List[sample.ClassDefinition]

  /**
    *  Converts a SIL function to a Sample method declaration.
    */
  def convert(function: sil.Function): sample.MethodDeclaration

  /**
    * Converts a SIL method to a Sample method declaration.
    */
  def convert(method: sil.Method): sample.MethodDeclaration

  /**
    *  Converts a SIL field to a Sample field declaration.
    */
  def convert(field: sil.Field): sample.FieldDeclaration

  /**
    *  Converts a SIL local variable to a Sample variable declaration.
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

  /**
    * Converts a sequence of SIL predicates to Sample predicates.
    *
    * The method only converts a SIL predicate if its shape is supported
    * by Samples predicate domain and if all of its nested predicate
    * could be converted as well. This is the reason why the method
    * takes a sequence of SIL predicates.
    */
  def convert(preds: Seq[sil.Predicate]): sample.PredicatesDomain
}

object DefaultSilverConverter extends SilverConverter with LazyLogging {
  var refType: sample.RefType = sample.RefType()
  var classDef: sample.ClassDefinition = null
  var prog: sil.Program = null

  def convert(p: sil.Program): List[sample.ClassDefinition] = {
    // Chicken-egg problem: To build the reference type,
    // we need its list of fields and to build the list of fields
    // (possibly reference fields), we need the reference type.
    refType = sample.RefType()
    refType.fields = p.fields.map(makeVariableIdentifier).toSet
    prog = p

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
      returnType = go(f.result.typ),
      body = f.body match {
        case None => // the function has no body
          val cfg = new sample.ControlFlowGraph(go(f.pos))
          cfg.addNode(resultVar :: Nil); cfg
        case Some(_) => // the function has a body
          val cfg = new sample.ControlFlowGraph(go(f.pos))
          val resultAssign = sample.Assignment(go(f.pos), resultVar, right = go(f.body.get))
          cfg.addNode(resultAssign :: resultVar :: Nil); cfg
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

        // put precondition into a separate block
        val prePp = if (m.pres.isEmpty) VirtualProgramPoint("precondition", m.pos) else go(m.pres.head.pos)
        val pre = makeNativeMethodCall(
          pos = prePp,
          name = SilverMethods.precondition.toString,
          args = Seq(makeConjunction(m.pres)),
          returnType = sample.TopType)
        val preBlock = cfg.addNode(pre :: Nil)

        // put local variable declarations into a separate block
        val varBlock = cfg.addNode(m.locals.map(go).toList)
        cfg.addEdge(preBlock, varBlock, None)

        // build cfg of body
        val bodyBlock = convertCfg(m.body.toCfg)(cfg)
        cfg.addEdge(varBlock, bodyBlock, None)

        // put postcondition into a separate block
        val leaves = cfg.getLeavesIds
        val postPp = if(m.posts.isEmpty) VirtualProgramPoint("postcondition", m.pos) else go(m.posts.head.pos)
        val post = makeNativeMethodCall(
          pos = postPp,
          name = SilverMethods.postcondition.toString,
          args = Seq(makeConjunction(m.posts)),
          returnType = sample.TopType)
        val postBlock = cfg.addNode(post :: Nil)
        leaves.foreach(cfg.addEdge(_, postBlock, None))

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
    case pos: sil.HasLineColumn => sample.WrappedProgramPoint(pos)
  }

  def convert(p: sil.Type): sample.Type = p match {
    case sil.Bool => sample.BoolType
    case sil.Int => sample.IntType
    case sil.Ref => refType
    case sil.DomainType(name,_) => sample.DomType(name)

    // Stubs
    case sil.Perm |
         sil.TypeVar(_) |
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
          pos = VirtualProgramPoint(field.name, s.pos),
          name = SilverMethods.inhale.toString,
          args = go(predicate) :: Nil,
          returnType = sample.TopType)
        // add inhale to list of statements
        statements ++ Seq(inhale)
      }

    case sil.FieldAssign(lhs, rhs) =>
      val assignment = sample.Assignment(go(s.pos), go(lhs), go(rhs))
      Seq(assignment)

    case sil.MethodCall(method, args, targets) =>
      val call = sample.MethodCall(
        pp = go(s.pos),
        method = makeVariable(s.pos, sil.Ref, method),
        parametricTypes = Nil,
        parameters = args.map(go).toList,
        returnedType = sample.TopType)
      Seq(call)

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

    case sil.Fresh(_) | sil.Constraining(_,_) | sil.Seqn(_) =>
      ???

    case sil.Goto(_) |
         sil.If(_, _, _) |
         sil.Label(_) |
         sil.While(_, _, _, _) =>
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
    case sil.Result() =>
      makeVariable(e.pos, e.typ, Constants.ResultVariableName)
    case sil.Unfolding(acc, inner) =>
      go(inner)

    // Access predicates
    // @author Caterina Urban

    case sil.FieldAccessPredicate(loc, perm) => perm match {
      case _: sil.FullPerm |
           _: sil.NoPerm |
           _: sil.LocalVar => makeNativeMethodCall(
        pos = go(e.pos),
        name = SilverMethods.permission.toString,
        args = go(loc) :: go(perm) :: sample.ConstantStatement(go(perm.pos), "1", sample.IntType) :: Nil,
        returnType = go(loc.typ))
      case perm: sil.FractionalPerm => makeNativeMethodCall(
        pos = go(e.pos),
        name = SilverMethods.permission.toString,
        args = go(loc) :: go(perm.left) :: go(perm.right) :: Nil,
        returnType = go(loc.typ))
      case add: sil.PermAdd =>
        // acc(a.f, p + q) -> acc(a.f, p) && acc(a.f, q)
        val left = sil.FieldAccessPredicate(loc, add.left)(e.pos)
        val right = sil.FieldAccessPredicate(loc, add.right)(e.pos)
        go(sil.And(left, right)(add.pos))
      case _ =>
        throw new NotImplementedError("A sil.PermExp conversion is missing!")
    }
    case e: sil.FullPerm => sample.ConstantStatement(go(e.pos), "1", sample.IntType)
    case e: sil.NoPerm => sample.ConstantStatement(go(e.pos), "0", sample.IntType)

    // SeqExp (e.g., data : Seq[Int]) are smashed into summary variables (e.g., data : Int)
    // their length (e.g., |data|) is treated as another unbounded variable
    // @author Caterina Urban

    case e: sil.SeqLength => // sequence length, e.g., |this.data|
      makeVariable(e.pos, e.typ, e.toString)
    case sil.SeqIndex(sil.FieldAccess(rcv, field),_) => // sequence access, e.g., this.data[i]
      sample.FieldAccess(go(e.pos),go(rcv),field.name,go(field.typ))
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

  def convert(preds: Seq[sil.Predicate]): sample.PredicatesDomain = {
    val predIdToBodyMap: Map[sample.PredicateIdentifier, sample.PredicateBody] =
      preds.map(convert).flatten.toMap

    /** Returns the set of all predicate IDs recursively nested
      * in the predicate with the given ID.
      */
    def deeplyNestedPredIds(
        predId: sample.PredicateIdentifier,
        foundSoFar: Set[sample.PredicateIdentifier] = Set.empty):
      Set[sample.PredicateIdentifier] = {

      if (foundSoFar.contains(predId)) {
        foundSoFar // Terminate when reaching an already handled predicate ID
      } else {
        // Get nested predicate IDs, if any
        val nestedPredIds = predIdToBodyMap.get(predId) match {
          case Some(predBody) => predBody.nestedPredIds
          case None => Set.empty
        }

        // Recurse to all nested predicate IDs
        nestedPredIds.foldLeft(foundSoFar + predId)({
          case (newFoundSoFar, nestedPredId) =>
            deeplyNestedPredIds(nestedPredId, newFoundSoFar)
        })
      }
    }

    // Filter out any predicates that could not be converted completely
    var result = sample.PredicatesDomain().top()
    for ((predId, predBody) <- predIdToBodyMap) {
      if (deeplyNestedPredIds(predId).subsetOf(predIdToBodyMap.keySet)) {
        result = result.add(predId, predBody)
      }
    }

    result
  }

  /** Converts a single SIL predicate to a Sample predicate, if possible.
    *
    * If the given predicate has a shape that our domain of predicates does not
    * support, the method returns `None`.
    */
  private def convert(pred: sil.Predicate): Option[(sample.PredicateIdentifier, sample.PredicateBody)] = {
    if (pred.formalArgs.map(_.typ) != Seq(sil.Ref)) {
      // Only support SIL predicates with a single reference parameter
      return None
    }

    val formalArgVar = pred.formalArgs.head.localVar

    // Reorder null-ness check expressions such that the null literals always
    // appear on the right side. Makes it easier to pattern-match later.
    val normalizedBody = pred.body.get.transform()(post = { // TODO: may not allow abstract predicates
      case n @ sil.NeCmp(left @ sil.NullLit(), right) =>
        n.copy(right, left)(n.pos, n.info)
    })

    // Maps each field with write permission to a set of predicate IDs
    var fieldsWithPerm = Map.empty[sample.Identifier, Set[sample.PredicateIdentifier]]

    // Predicate must be a conjunction of constituents we support, that is,
    // field access predicates, and conditional predicate access predicates
    flattenConjunction(normalizedBody).foreach({
      case sil.FieldAccessPredicate(sil.FieldAccess(rcv, field), sil.FullPerm())
        if formalArgVar == rcv =>
        // Found a field access predicate for a field of the formal argument
        fieldsWithPerm += makeVariableIdentifier(field) -> Set.empty
      case implies @ sil.Implies(
      sil.NeCmp(fa @ sil.FieldAccess(rcv, field), sil.NullLit()),
      sil.PredicateAccessPredicate(sil.PredicateAccess(args, nestedPred), sil.FullPerm()))
        if formalArgVar == rcv && args == Seq(fa) =>
        // Found a nested predicate access predicate for a field
        // of the formal argument
        val nestedPredId = new sample.PredicateIdentifier(nestedPred)
        fieldsWithPerm += makeVariableIdentifier(field) -> Set(nestedPredId)
      case n =>
        // Give up if the predicate contains anything else
        logger.warn(s"Cannot handle constituent $n of predicate ${pred.name}")
        return None
    })

    val samplePredId = new sample.PredicateIdentifier(pred.name)
    val samplePredBody = sample.PredicateBody().functionalFactory(
      fieldsWithPerm.mapValues(predIds => {
        predIds.foldLeft(sample.NestedPredicatesDomain())(_.+(_))
      }))
    Some(samplePredId -> samplePredBody)
  }

  /**
   * Converts a SIL CFG block, adds it to the given Sample CFG and recurses to its successors.
    *
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
          b.stmt.children.flatMap(go).toList ++ (go(b.cond) :: Nil)
        case b: sil.StatementBlock =>
          b.stmt.children.flatMap(go).toList
        case lb: sil.LoopBlock =>
          // generate method call for all invariants
          val pp = if (lb.invs.isEmpty) VirtualProgramPoint("invariant", lb.pos) else go(lb.invs.head.pos)
          val invariants = makeNativeMethodCall(
            pos = pp,
            name = SilverMethods.invariant.toString,
            args = Seq(makeConjunction(lb.invs)),
            returnType = sample.TopType)
          invariants :: go(lb.cond) :: Nil
        case b: sil.Fresh =>
          sample.EmptyStatement(sample.DummyProgramPoint) :: Nil
        case b: sil.Constraining =>
          sample.EmptyStatement(sample.DummyProgramPoint) :: Nil
        //case b: sil.ConstrainingBlock =>
        //  throw new IllegalArgumentException("The implementation for sil.ConstrainingBlock is missing.")
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
            // Only add a back-edge if there is no back edge already. If there
            // is a back edge then it is the terminal block of an inner loop.
            val terminalIndex = convertCfg(terminalBlock)
            val hasBackEdge = cfg.edges.exists { case (from, _, _) => from == terminalIndex}
            if (!hasBackEdge) cfg.addEdge(terminalIndex, index, None)
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

  private def go(p: sil.Predicate) = convert(p)
}