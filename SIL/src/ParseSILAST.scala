import ch.ethz.inf.pm.sample.abstractdomain.{Identifier, VariableIdentifier}
import ch.ethz.inf.pm.sample.oorepresentation._
import java.lang.Exception
import silAST.expressions._
import silAST.methods._
import implementations._
import silAST.programs._
import silAST.source._
import silAST.symbols.logical._
import silAST.symbols.logical.quantification._
import silAST.types._
import symbols._
import terms._


object ParseSILAST {
  def SILProgram2SimpleProgram(program : Program) : Set[MethodDeclaration]= {
    parseMethods(program.methods)
    //TODO: all the rest of the program
  }

  private def parseMethods( m : scala.collection.Set[Method]) : Set[MethodDeclaration] = {
    var result : Set[MethodDeclaration] = Set.empty;
    for(method <- m)
      result ++= parseMethod(method);
    return result;
  }

  private def parseMethod(m : Method) : Set[MethodDeclaration] = {
    val pp = parsePP(m.sourceLocation)
    val ownerType : Type = null;
    val modifiers : List[Modifier] = List.empty[Modifier];
    val name : MethodIdentifier = parseMethodName(m.name);
    val parametricType : List[Type] = List.empty[Type];
    val arguments : List[List[VariableDeclaration]] = raiseList(
      parseSeqOf[ProgramVariable, VariableDeclaration](m.signature.parameters.variables, parseVariableDeclaration(_)));
    val returnedVariables : List[VariableDeclaration] = parseSeqOf[ProgramVariable, VariableDeclaration](m.signature.parameters.variables, parseVariableDeclaration(_))
    if(returnedVariables.size>1) throw new SILParserException("Not yet supported")
    val returnedType : Type = returnedVariables.iterator.next().typ;
    val precondition : ch.ethz.inf.pm.sample.oorepresentation.Statement = parseExpressionSequence(m.signature.precondition, parsePP(m.signature.precondition.sourceLocation))
    val postcondition : ch.ethz.inf.pm.sample.oorepresentation.Statement= parseExpressionSequence(m.signature.postcondition, parsePP(m.signature.postcondition.sourceLocation))
    val cfgs : Set[ch.ethz.inf.pm.sample.oorepresentation.ControlFlowGraph] = parseSetOf[Implementation, ch.ethz.inf.pm.sample.oorepresentation.ControlFlowGraph](m.implementations.asInstanceOf[Set[Implementation]], parseImplementation(_))
    var result : Set[MethodDeclaration] = Set.empty[MethodDeclaration];
    for(cfg <- cfgs)
      result+=new MethodDeclaration(pp, ownerType, modifiers, name, parametricType, arguments, returnedType,cfg, precondition, postcondition);
    return result;
  }

  private def parseSeqOf[T, S](l : Seq[T], f : T => S) : List[S] = l match {
    case Nil => return Nil;
    case x :: xs => return f(x) :: parseSeqOf (xs, f);
  }


  private def parseSetOf[T, S](l : Set[T], f : T => S) : Set[S] = {
    var result : Set[S] = Set.empty;
    for(m <- l)
      result += f(m);
    return result;
  }

  //TODO: wait to have row and column
  private def parsePP(s : SourceLocation) : ProgramPoint = s match {
    case a : TypeSubstitutedSourceLocation => new SILProgramPoint(-1, -1)
    case a : LogicalSubstitutedSourceLocation => new SILProgramPoint(-1, -1)
    case a : PVSubstitutedSourceLocation => new SILProgramPoint(-1, -1)
    case noLocation => throw new SILParserException("We need a location here")
  }

  private def parseMethodName(s : String) = new SILMethodIdentifier(s)

  private def flattenList[T](l : List[List[T]]) : List[T] = l match {
    case Nil => return Nil;
    case x :: xs => x match {
      case y :: Nil => return y :: flattenList(xs);
      case _ => throw new SILParserException("This should not happen");
    }
  }

  private def raiseList[T](l : List[T]) : List[List[T]] = l match {
    case Nil => return Nil;
    case x :: xs => x match {
      case y :: Nil => return ((y :: Nil) :: raiseList(xs)).asInstanceOf[List[List[T]]];
      case _ => throw new SILParserException("This should not happen");
    }
  }

  private def parseVariableDeclaration(p : symbols.Variable) : VariableDeclaration = {
    val pp : ProgramPoint = this.parsePP(p.sourceLocation);
    val typ : Type = this.parseDataType(p.dataType)
    val variable : ch.ethz.inf.pm.sample.oorepresentation.Variable = new ch.ethz.inf.pm.sample.oorepresentation.Variable(pp, new VariableIdentifier(p.name, typ, pp))
    val right : ch.ethz.inf.pm.sample.oorepresentation.Statement = null
    return new VariableDeclaration(pp, variable, typ, right);
  }


  private def parseVariable(p : symbols.Variable) : ch.ethz.inf.pm.sample.oorepresentation.Variable = {
    val pp : ProgramPoint = this.parsePP(p.sourceLocation);
    val typ : Type = this.parseDataType(p.dataType)
    return new ch.ethz.inf.pm.sample.oorepresentation.Variable(pp, new VariableIdentifier(p.name, typ, pp))
  }

  private def parseDataType(p : DataType) : SILType = p match {
    case x : VariableType => return new SILType(x.variable.name)
    case x : NonReferenceDataType => return new SILType(x.domain.name)
    case x : ReferenceDataType => return new SILType(x.domain.name)
  }


  private def parseExpressionSequence(p : Seq[silAST.expressions.Expression], pp : ProgramPoint) : ch.ethz.inf.pm.sample.oorepresentation.Statement= {
    if (p.size==0) return new ConstantStatement(pp, "true", new SILType("Boolean"));
    if (p.size==1) return parseExpression(p.apply(0));
    return new MethodCall(pp,
      new FieldAccess(pp, parseExpression(p.apply(0)) :: Nil, "&&",null),
        Nil,
      parseExpressionSequence(p.drop(0), pp) :: Nil,
      new SILType("Boolean")
    )
  }

  private def parseExpression(p : silAST.expressions.Expression) : ch.ethz.inf.pm.sample.oorepresentation.Statement= p match {

    //Predicate&C expressions
    case x : silAST.expressions.FieldPermissionExpression =>
      return new MethodCall(parsePP(x.sourceLocation),
        new FieldAccess(parsePP(x.sourceLocation), parseFieldLocation(x.location) :: Nil, "fieldPermission",null),
        Nil,
        parseTerm(x.permission)::Nil,
        new SILType("Chalice")
      )
      //Correspond to new FieldPermissionExpression(parsePP(p.sourceLocation), parseFieldLocation(x.location), parseTerm(x.permission));
    case x : silAST.expressions.PredicatePermissionExpression =>
      return new MethodCall(parsePP(x.sourceLocation),
        new FieldAccess(parsePP(x.sourceLocation), parsePredicateLocation(x.location) :: Nil, "predicatePermission",null),
        Nil,
        parseTerm(x.permission)::Nil,
        new SILType("Chalice")
      )
      //Correspond to return new PredicatePermissionExpression(parsePP(p.sourceLocation), parsePredicateLocation(x.location), parseTerm(x.permission));
    case x : silAST.expressions.OldExpression =>
      return new MethodCall(parsePP(x.sourceLocation),
        new FieldAccess(parsePP(x.sourceLocation), parseExpression(x.expression) :: Nil, "old",null),
        Nil,
        Nil,
        new SILType("Chalice")
      )
      //Correspond to new OldExpression(parsePP(x.sourceLocation), parseExpression(x.expression))
    case x : silAST.expressions.UnfoldingExpression =>
      return new MethodCall(parsePP(x.sourceLocation),
        new FieldAccess(parsePP(x.sourceLocation), parseExpression(x.expression) :: Nil, "unfolding",null),
        Nil,
        parseExpression(x.location) :: Nil,
        new SILType("Chalice")
      )
      //Correspond to new UnfoldingExpression(parsePP(x.sourceLocation), parseExpression(x.location).asInstanceOf[PredicatePermissionExpression],parseExpression(x.expression))
    case x : silAST.expressions.DomainPredicateExpression =>  throw new SILParserException("Not yet supported")
      //Correspond to new DomainPredicateExpression(parsePP(x.sourceLocation), x.predicate, parseSeqOf[Term, ch.ethz.inf.pm.sample.abstractdomain.Expression](x.arguments, parseTerm(_)));

    //Boolean expressions
    case x : QuantifierExpression => x.quantifier match {
      case y : Forall =>
        return new MethodCall(parsePP(x.sourceLocation),
          new FieldAccess(parsePP(x.sourceLocation), parseExpression(x.expression) :: Nil, "forall",null),
          Nil,
          parseVariable(x.variable) :: Nil,
          new SILType("Boolean")
        )
      //Correspond to new ForAllExpression(parsePP(x.sourceLocation), parseVariable(x.variable), parseExpression(x.expression));
      case y : Exists =>
        return new MethodCall(parsePP(x.sourceLocation),
          new FieldAccess(parsePP(x.sourceLocation), parseExpression(x.expression) :: Nil, "exists",null),
          Nil,
          parseVariable(x.variable) :: Nil,
          new SILType("Boolean")
        )
      //Correspond to new ExistExpression(parsePP(x.sourceLocation), parseVariable(x.variable), parseExpression(x.expression));
    }
    case x : EqualityExpression =>
      return new MethodCall(parsePP(x.sourceLocation),
        new FieldAccess(parsePP(x.sourceLocation), parseTerm(x.term1) :: Nil, "==",null),
        Nil,
        parseTerm(x.term2) :: Nil,
        new SILType("Boolean")
      )
      //Correspond to new ReferenceComparisonExpression(parseTerm(x.term1), parseTerm(x.term2), ArithmeticOperator.==, new SILType("Boolean"));
    case x : UnaryExpression => x.operator match {
      case y : Not =>
        return new MethodCall(parsePP(x.sourceLocation),
          new FieldAccess(parsePP(x.sourceLocation), parseExpression(x.operand1) :: Nil, "!",null),
          Nil,
          Nil,
          new SILType("Boolean")
        )
        //Correspond to new NegatedBooleanExpression(parseExpression(x.operand1))
    }
    case x : BinaryExpression => x.operator match {
      case y : Or =>
        return new MethodCall(parsePP(x.sourceLocation),
          new FieldAccess(parsePP(x.sourceLocation), parseExpression(x.operand1) :: Nil, "||",null),
          Nil,
          parseExpression(x.operand2) :: Nil,
          new SILType("Boolean")
        )
        //Correspond to new BinaryBooleanExpression(parseExpression(x.operand1), parseExpression(x.operand2), BooleanOperator.||, new SILType("Boolean"))
      case y : And =>
        return new MethodCall(parsePP(x.sourceLocation),
          new FieldAccess(parsePP(x.sourceLocation), parseExpression(x.operand1) :: Nil, "&&",null),
            Nil,
          parseExpression(x.operand2) :: Nil,
          new SILType("Boolean")
        )
        //Correspond to new BinaryBooleanExpression(parseExpression(x.operand1), parseExpression(x.operand2), BooleanOperator.&&, new SILType("Boolean"))
      case y : Implication =>
        return new MethodCall(parsePP(x.sourceLocation),
          new FieldAccess(parsePP(x.sourceLocation), parseExpression(x.operand1) :: Nil, "==>",null),
            Nil,
          parseExpression(x.operand2) :: Nil,
          new SILType("Boolean")
        )
        //Correspond to new BinaryBooleanExpression(parseExpression(x.operand1), parseExpression(x.operand2), BooleanOperator.==>, new SILType("Boolean"))
      case y : Equivalence =>
        return new MethodCall(parsePP(x.sourceLocation),
          new FieldAccess(parsePP(x.sourceLocation), parseExpression(x.operand1) :: Nil, "<==>",null),
            Nil,
          parseExpression(x.operand2) :: Nil,
          new SILType("Boolean")
        )
        //Correspond to new BinaryBooleanExpression(parseExpression(x.operand1), parseExpression(x.operand2), BooleanOperator.==>, new SILType("Boolean"))
    }
    case x : silAST.expressions.TrueExpression => new ConstantStatement(parsePP(p.sourceLocation), "true", new SILType("Boolean"))
      //Correspond to new ch.ethz.inf.pm.sample.abstractdomain.TrueExpression(parsePP(p.sourceLocation), new SILType("Boolean"));
    case x : silAST.expressions.FalseExpression => new ConstantStatement(parsePP(p.sourceLocation), "false", new SILType("Boolean"))
      //Correspond to new ch.ethz.inf.pm.sample.abstractdomain.FalseExpression(parsePP(p.sourceLocation), new SILType("Boolean"));
  }

  private def parseTerm(p : Term) : ch.ethz.inf.pm.sample.oorepresentation.Statement = p match {
    case x : ProgramVariableTerm => return parseVariable(x.variable)
    case x : OldTerm =>
      return new MethodCall(parsePP(x.sourceLocation),
        new FieldAccess(parsePP(x.sourceLocation), parseTerm(x.term) :: Nil, "old",null),
        Nil,
        Nil,
        new SILType("Chalice")
      )
    //Correspond to new OldExpression(parsePP(x.sourceLocation), parseTerm(x.term))
    case x : UnfoldingTerm =>
      return new MethodCall(parsePP(x.sourceLocation),
        new FieldAccess(parsePP(x.sourceLocation), parseTerm(x.term) :: Nil, "unfolding",null),
        Nil,
        parseExpression(x.predicate) :: Nil,
        new SILType("Chalice")
      )
    //Correspond to new UnfoldingExpression(parsePP(x.sourceLocation), parseExpression(x.predicate).asInstanceOf[PredicatePermissionExpression], parseTerm(x.term))
    case x : CastTerm =>
      return new MethodCall(parsePP(x.sourceLocation),
        new FieldAccess(parsePP(x.sourceLocation), parseTerm(x.operand1) :: Nil, "$asInstanceOf",null),
        parseDataType(x.newType) :: Nil,
        Nil,
        new SILType("Any")
      )
    case x : EpsilonPermissionTerm => new ConstantStatement(parsePP(x.sourceLocation), "epsilon", new SILType("PermissionValue"))
    case x : FullPermissionTerm => new ConstantStatement(parsePP(x.sourceLocation), "full", new SILType("PermissionValue"))
    case x : NoPermissionTerm => new ConstantStatement(parsePP(x.sourceLocation), "zero", new SILType("PermissionValue"))
    case x : PermTerm =>
      return new MethodCall(parsePP(x.sourceLocation),
        new FieldAccess(
          parsePP(x.sourceLocation),
          new FieldAccess(parsePP(x.sourceLocation), parseTerm(x.location.receiver) :: Nil, x.location.asInstanceOf[FieldLocation].field.name,null) :: Nil,
          "getPermission",
          null
        ),
        Nil,
        Nil,
        new SILType("Chalice")
      )
    case x : FieldLocation => parseFieldLocation(x)
    case x : FieldReadTerm => new FieldAccess(parsePP(x.sourceLocation), parseTerm(x.location.receiver) :: Nil, x.location.field.name, parseDataType(x.location.field.dataType))
    case x : FunctionApplicationTerm =>
      return new MethodCall(parsePP(x.sourceLocation),
        new FieldAccess(parsePP(x.sourceLocation), parseTerm(x.receiver) :: Nil, x.function.name,null),
        Nil,
        parseSeqOf[Term, ch.ethz.inf.pm.sample.oorepresentation.Statement](x.arguments, parseTerm(_)),
        parseDataType(x.function.resultType)
      )
    case x : IntegerLiteralTerm => new ConstantStatement(parsePP(x.sourceLocation), x.value.toString(), parseDataType(x.dataType))
    case x : LogicalVariableTerm => parseVariable(x.variable)
    case x : DomainFunctionApplicationTerm => throw new SILParserException("Not yet supported")
    case x : IfThenElseTerm => throw new SILParserException("Not yet supported")
  }


  private def parseStatement(p : silAST.methods.implementations.Statement) : ch.ethz.inf.pm.sample.oorepresentation.Statement= p match {
    //Standard statements
    case x : AssignmentStatement => return new Assignment(parsePP(x.sourceLocation), parseVariable(x.target), parseTerm(x.source))
    case x : CallStatement =>
      if(x.method.signature.results.size!=1) throw new SILParserException("Not yet supported")
      return new MethodCall(parsePP(x.sourceLocation),
        new FieldAccess(parsePP(x.sourceLocation), parseProgramVariableSequence(x.targets)::Nil, x.method.name, null),
        Nil,
        parseSeqOf[Term, ch.ethz.inf.pm.sample.oorepresentation.Statement](x.arguments.args, parseTerm(_)),
        parseDataType(x.method.signature.results.apply(1).dataType)
      )
    case x : NewStatement => return new New(parsePP(x.sourceLocation), parseDataType(x.dataType));
    case x : FieldAssignmentStatement =>
      return new Assignment(
        parsePP(x.sourceLocation),
        new FieldAccess(parsePP(x.sourceLocation), parseVariable(x.target)::Nil, x.field.name, null),
        parseTerm(x.source)
      )

    //Chalice/SIL statements represented by method calls
    case x : ExhaleStatement =>
      return new MethodCall(parsePP(x.sourceLocation),
        new ConstantStatement(parsePP(x.sourceLocation),"exhale",new SILType("Unit")),
        Nil,
        parseExpression(x.expression) :: Nil,
        new SILType("Unit")
      )
    case x : FoldStatement =>
      return new MethodCall(parsePP(x.sourceLocation),
        new ConstantStatement(parsePP(x.sourceLocation),"fold",new SILType("Unit")),
        Nil,
        parseTerm(x.permission) :: Nil,
        new SILType("Unit")
      )
    case x : InhaleStatement =>
      return new MethodCall(parsePP(x.sourceLocation),
        new ConstantStatement(parsePP(x.sourceLocation),"inhale",new SILType("Unit")),
        Nil,
        parseExpression(x.expression) :: Nil,
        new SILType("Unit")
      )
    case x : UnfoldStatement =>
      return new MethodCall(parsePP(x.sourceLocation),
        new ConstantStatement(parsePP(x.sourceLocation),"unfold",new SILType("Unit")),
        Nil,
        parseExpression(x.permissionExpression) :: Nil,
        new SILType("Unit")
      )
  }


  private def parseProgramVariableSequence(p : Seq[ProgramVariable]) : ch.ethz.inf.pm.sample.oorepresentation.Statement = p match {
    case x :: y :: Nil => new FieldAccess(parsePP(x.sourceLocation), parseVariable(x) :: Nil, y.name, parseDataType(y.dataType))
    case x :: Nil => parseVariable(x)
    case y => new FieldAccess(parsePP(y.apply(0).sourceLocation), parseProgramVariableSequence(y.drop(y.size-1)) :: Nil, y.apply(y.size-1).name, parseDataType(y.apply(y.size-1).dataType))
  }


  private def parsePredicateLocation(x : PredicateLocation) : ch.ethz.inf.pm.sample.oorepresentation.Statement =
    new FieldAccess(parsePP(x.sourceLocation), parseTerm(x.receiver) :: Nil, x.predicate.name, parseDataType(x.dataType))

  private def parseFieldLocation(x : FieldLocation) : ch.ethz.inf.pm.sample.oorepresentation.Statement =
    new FieldAccess(parsePP(x.sourceLocation), parseTerm(x.receiver) :: Nil, x.field.name, parseDataType(x.field.dataType))

  private def parseImplementation(p : Implementation) : ch.ethz.inf.pm.sample.oorepresentation.ControlFlowGraph = parseControlFlowGraph(p.body)

  private def parseControlFlowGraph(p : silAST.methods.implementations.ControlFlowGraph) : ch.ethz.inf.pm.sample.oorepresentation.ControlFlowGraph= {
    var (blocksindexes, edges, cfg, entryIndex) = parseInternalControlFlowGraph(p, Map.empty[Block, Int], Set.empty[CFGEdge], new ch.ethz.inf.pm.sample.oorepresentation.ControlFlowGraph(parsePP(p.sourceLocation)));
    for(edge <- edges)
      cfg.addEdge(blocksindexes.apply(edge.source), blocksindexes.apply(edge.target), Some(true));
    //TODO: should I set the entry node or it's always the first node?
    return cfg;
  }


  private def parseInternalControlFlowGraph(p : silAST.methods.implementations.ControlFlowGraph, b : Map[Block, Int], e : Set[CFGEdge], c : ch.ethz.inf.pm.sample.oorepresentation.ControlFlowGraph) : (Map[Block, Int], Set[CFGEdge], ch.ethz.inf.pm.sample.oorepresentation.ControlFlowGraph, Int)= {
    var cfg : ch.ethz.inf.pm.sample.oorepresentation.ControlFlowGraph = c;
    var blocksindexes = b;
    var edges : Set[CFGEdge] = e;
    var initialindex : Int = -1;
    for(b <- p.nodes) {
      edges=edges++b.predecessors++b.successors;
      b match {
        case c : BasicBlock =>
          val index=cfg.addNode(parseBasicBlock(c.statements));
          blocksindexes=blocksindexes+((c, index));
          if(initialindex== -1) initialindex=index;
        case c : LoopBlock =>
          val conditionindex = cfg.addNode(parseExpression(c.condition)::Nil);
          if(initialindex== -1) initialindex=conditionindex;
          val (b1, e1, c1, newindex)=parseInternalControlFlowGraph(c.body, blocksindexes, edges, cfg);
          blocksindexes=b1;
          edges=e1;
          cfg=c1;
          cfg.addEdge(conditionindex, newindex, None);
      }
    }
    return (blocksindexes, edges, cfg, initialindex)
  }



  private def parseBasicBlock(p : Seq[silAST.methods.implementations.Statement]) : List[ch.ethz.inf.pm.sample.oorepresentation.Statement] = {
    var result : List[ch.ethz.inf.pm.sample.oorepresentation.Statement] = Nil;
    for(i <- 0 to p.size-1) {
      if(i != p.size-1 || p.apply(i).isInstanceOf[ControlStatement])
        result=result ::: parseStatement(p.apply(i)) :: Nil;
    }
    return result;
  }

}

class SILProgramPoint(val row : Int, val column : Int) extends LineColumnProgramPoint {
  def getLine() : Int = row;
  def getColumn() : Int = column;
}

class SILMethodIdentifier(val name : String) extends MethodIdentifier

class SILType(val name : String) extends Type {

  def isFloatingPointType() : Boolean = throw new SILParserException("Not yet implemented")
  def isObject() : Boolean = throw new SILParserException("Not yet implemented")
  def isNumericalType() : Boolean = throw new SILParserException("Not yet implemented")
  def isStringType() : Boolean = throw new SILParserException("Not yet implemented")
  def isStatic() : Boolean = throw new SILParserException("Not yet implemented")
  def getName() : String = throw new SILParserException("Not yet implemented")
  def getPossibleFields() : Set[Identifier] = throw new SILParserException("Not yet implemented")
  def getArrayElementsType() : Option[Type] = throw new SILParserException("Not yet implemented")
  def isBottomExcluding(types : Set[Type]) : Boolean = throw new SILParserException("Not yet implemented")
  def factory() = throw new SILParserException("Not yet implemented")
  def top() = throw new SILParserException("Not yet implemented")
  def bottom() = throw new SILParserException("Not yet implemented")
  def lub(left : Type, right : Type) = throw new SILParserException("Not yet implemented")
  def glb(left : Type, right : Type) = throw new SILParserException("Not yet implemented")
  def widening(left : Type, right : Type) = throw new SILParserException("Not yet implemented")
  def lessEqual(r : Type) : Boolean = throw new SILParserException("Not yet implemented")
}

class SILParserException(s : String) extends Exception(s)