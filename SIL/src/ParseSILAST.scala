import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import java.lang.Exception
import silAST.expressions._
import silAST.expressions.util._
import silAST.methods._
import implementations.Implementation
import silAST.programs._
import silAST.source._
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
      parseSeqOf[ProgramVariable, VariableDeclaration](m.signature.parameters.variables, parseProgramVariable(_)));
    val returnedVariables : List[VariableDeclaration] = parseSeqOf[ProgramVariable, VariableDeclaration](m.signature.parameters.variables, parseProgramVariable(_))
    if(returnedVariables.size>1) throw new SILParserException("Not yet supported")
    val returnedType : Type = returnedVariables.iterator.next().typ;
    val precondition : ch.ethz.inf.pm.sample.abstractdomain.Expression = parseExpression(m.signature.precondition)
    val postcondition : ch.ethz.inf.pm.sample.abstractdomain.Expression = parseExpression(m.signature.postcondition)
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

  private def parseProgramVariable(p : ProgramVariable) : VariableDeclaration = {
    val pp : ProgramPoint = this.parsePP(p.sourceLocation);
    val typ : Type = this.parseDataType(p.dataType)
    val variable : ch.ethz.inf.pm.sample.oorepresentation.Variable = new ch.ethz.inf.pm.sample.oorepresentation.Variable(pp, new VariableIdentifier(p.name, typ, pp))
    val right : Statement = null
    return new VariableDeclaration(pp, variable, typ, right);
  }

  private def parseDataType(p : DataType) : SILType = p match {
    case x : VariableType => return new SILType(x.variable.name)
    case x : NonReferenceDataType => return new SILType(x.domain.name)
    case x : ReferenceDataType => return new SILType(x.domain.name)
  }


  private def parseExpression(p : ExpressionSequence) : ch.ethz.inf.pm.sample.abstractdomain.Expression= null

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