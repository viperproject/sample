import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import java.lang.Exception
import silAST.expressions._
import silAST.expressions.util._
import silAST.methods._
import implementations.Implementation
import silAST.programs._
import silAST.source._
import silAST.symbols.logical.quantification.{Exists, Forall}
import silAST.types._
import symbols._


object ParseSILAST {
  def SILProgram2SimpleProgram(program : Program) : Set[MethodDeclaration]= {
    parseMethods(program.methods)
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
    val precondition : ch.ethz.inf.pm.sample.abstractdomain.Expression = parseExpressionSequence(m.signature.precondition, parsePP(m.signature.precondition.sourceLocation))
    val postcondition : ch.ethz.inf.pm.sample.abstractdomain.Expression = parseExpressionSequence(m.signature.postcondition, parsePP(m.signature.postcondition.sourceLocation))
    val cfgs : Set[ControlFlowGraph] = parseSetOf[Implementation, ControlFlowGraph](m.implementations.asInstanceOf[Set[Implementation]], parseImplementation(_))
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
    val right : Statement = null
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


  private def parseExpressionSequence(p : Seq[silAST.expressions.Expression], pp : ProgramPoint) : ch.ethz.inf.pm.sample.abstractdomain.Expression= {
    if (p.size==0) return new ch.ethz.inf.pm.sample.abstractdomain.TrueExpression(pp, new SILType("Boolean"));
    if (p.size==1) return parseExpression(p.apply(0));
    return new BinaryBooleanExpression(parseExpression(p.apply(0)), parseExpressionSequence(p.drop(0), pp), BooleanOperator.&&, new SILType("Boolean"));
  }

  private def parseExpression(p : silAST.expressions.Expression) : ch.ethz.inf.pm.sample.abstractdomain.Expression= p match {
    case x : FieldPermissionExpression => throw new SILParserException("Not yet implemented")
    case x : PredicatePermissionExpression => throw new SILParserException("Not yet implemented")
    case x : OldExpression => throw new SILParserException("Not yet implemented")
    case x : UnfoldingExpression => throw new SILParserException("Not yet implemented")
    case x : EqualityExpression => throw new SILParserException("Not yet implemented")
    case x : UnaryExpression => throw new SILParserException("Not yet implemented")
    case x : BinaryExpression => throw new SILParserException("Not yet implemented")
    case x : DomainPredicateExpression => throw new SILParserException("Not yet implemented")
    case x : QuantifierExpression => x.quantifier match {
      case y : Forall => return new ForAllExpression(parsePP(x.sourceLocation), parseVariable(x.variable), parseExpression(x.expression));
      case y : Exists => return new ExistExpression(parsePP(x.sourceLocation), parseVariable(x.variable), parseExpression(x.expression));
    }
    case x : silAST.expressions.TrueExpression => return new ch.ethz.inf.pm.sample.abstractdomain.TrueExpression(parsePP(p.sourceLocation), new SILType("Boolean"));
    case x : silAST.expressions.FalseExpression => return new ch.ethz.inf.pm.sample.abstractdomain.FalseExpression(parsePP(p.sourceLocation), new SILType("Boolean"));

    //NOT VISIBLE
    //case x : PEqualityExpressionC => throw new SILParserException("Not yet implemented")
    //case x : PUnaryExpressionC => throw new SILParserException("Not yet implemented")
    //case x : PBinaryExpressionC => throw new SILParserException("Not yet implemented")
    //case x : PDomainPredicateExpressionC => throw new SILParserException("Not yet implemented")
    //case x : DEqualityExpressionC => throw new SILParserException("Not yet implemented")
    //case x : DUnaryExpressionC => throw new SILParserException("Not yet implemented")
    //case x : DBinaryExpressionC => throw new SILParserException("Not yet implemented")
    //case x : DDomainPredicateExpressionC => throw new SILParserException("Not yet implemented")

    //ALREADY COVERED
    //case x : PDomainPredicateExpression => throw new SILParserException("Not yet implemented")
    //case x : PPredicatePermissionExpression => throw new SILParserException("Not yet implemented")
    //case x : PFieldPermissionExpression => throw new SILParserException("Not yet implemented")
    //case x : PUnfoldingExpression => throw new SILParserException("Not yet implemented")
    //case x : DQuantifierExpression => throw new SILParserException("Not yet implemented")
    //case x : GEqualityExpression => throw new SILParserException("Not yet implemented")
    //case x : GUnaryExpression => throw new SILParserException("Not yet implemented")
    //case x : GBinaryExpression => throw new SILParserException("Not yet implemented")
    //case x : GDomainPredicateExpression => throw new SILParserException("Not yet implemented")
  }

  private def parseImplementation(p : Implementation) : ch.ethz.inf.pm.sample.oorepresentation.ControlFlowGraph= null
}

class SILProgramPoint(val row : Int, val column : Int) extends ProgramPoint {
  def getLine() : Int = row;
  def getColumn() : Int = column;
}

class SILMethodIdentifier(val name : String) extends MethodIdentifier

class SILType(val name : String) extends Type {

  def isObject() : Boolean = throw new SILParserException("Not yet implemented")
  def isNumericalType() : Boolean = throw new SILParserException("Not yet implemented")
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