package ch.ethz.inf.pm.td.compiler

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.td._
import parser.{TypeName, ExpressionStatement}
import util.parsing.input.Position
import ch.ethz.inf.pm.sample.SystemParameters

/**
 *
 * Lucas Brutschy
 * Date: 8/22/12
 * Time: 4:21 PM
 *
 */
object CFGGenerator {

  var curScriptName:String = ""

  def process(script:parser.Script, scriptName:String):ClassDefinition = {
    curScriptName = scriptIdent(scriptName)
    val id = scriptIdent(scriptName)
    SystemParameters.typ=TouchType(scriptName,true)
    val programPoint : ProgramPoint = TouchProgramPoint(script.pos)
    val typ : Type = typeNameToType(TypeName(id), true)
    val modifiers : List[Modifier] = Nil
    val name : ClassIdentifier = TouchClassIdentifier(id,typ)
    val parametricTypes : List[Type] = Nil
    val extend : List[ClassIdentifier] = Nil
    val fields : List[FieldDeclaration] = findFields(script)
    val methods : List[MethodDeclaration] = findMethods(script,typ)
    val pack : PackageIdentifier = TouchPackageIdentifier()
    val inv : Expression = null
    new ClassDefinition(programPoint, typ, modifiers, name, parametricTypes, extend, fields, methods, pack, inv)
  }

  private def findFields(script:parser.Script):List[FieldDeclaration] = {
    (for (dec <- script.declarations) yield {
      dec match {
        case v@parser.VariableDefinition(variable,flags) =>
          val programPoint : ProgramPoint = TouchProgramPoint(v.pos)
          val modifiers : List[Modifier] = Nil
          val name : Variable = parameterToVariable(variable)
          val typ : Type = typeNameToType(variable.typeName)
          val right : Statement = null
          Some(new FieldDeclaration(programPoint,modifiers,name,typ,right))
        case _ => None
      }
    }).flatten
  }

  private def findMethods(script:parser.Script,ownerType:Type):List[MethodDeclaration] = {
    (for (dec <- script.declarations) yield {
      dec match {
        case act@parser.ActionDefinition(ident,in,out,body) =>
          val programPoint : ProgramPoint = TouchProgramPoint(act.pos)
          val modifiers : List[Modifier] = Nil
          val name : MethodIdentifier = TouchMethodIdentifier(ident)
          val parametricType : List[Type] = Nil
          val arguments : List[List[VariableDeclaration]] = List(in map (parameterToVariableDeclaration _))
          val returnType : Type = null
          val newBody : ControlFlowGraph = new ControlFlowGraph(programPoint)
          addStatementsToCFG(body,newBody)
          val preCond : Expression = null
          val postCond : Expression = null
          Some(new MethodDeclaration(programPoint,ownerType,modifiers,name,parametricType,arguments,returnType,newBody,preCond,postCond))
        case _ => None
      }
    }).flatten
  }

  private def parameterToVariableDeclaration(parameter:parser.Parameter):VariableDeclaration = {
    val programPoint : ProgramPoint = TouchProgramPoint(parameter.pos)
    val variable : Variable = parameterToVariable(parameter)
    val typ : Type = typeNameToType(parameter.typeName)
    val right : Statement = null
    VariableDeclaration(programPoint,variable,typ,right)
  }

  private def parameterToVariable(parameter:parser.Parameter):Variable = {
    val programPoint : ProgramPoint = TouchProgramPoint(parameter.pos)
    val id : Identifier = parameterToVariableIdentifier(parameter)
    Variable(programPoint,id)
  }

  private def parameterToVariableIdentifier(parameter:parser.Parameter):VariableIdentifier = {
    val name : String = parameter.ident
    val typ : Type = typeNameToType(parameter.typeName)
    val programPoint : ProgramPoint = TouchProgramPoint(parameter.pos)
    VariableIdentifier(name,typ,programPoint)
  }

  private def typeNameToType(typeName:parser.TypeName, isSingleton:Boolean = false):Type = {
    TouchType(typeName.ident, isSingleton)
  }

  private def addStatementsToCFG(statements:List[parser.Statement], cfg:ControlFlowGraph):(Int,Int) = {

    val firstNode = cfg.addNode(Nil)
    var newStatements:List[Statement] = Nil
    var curNode = firstNode

    for (statement <- statements) statement match {

      case parser.MetaStatement(_,_) =>
        Unit

      case parser.Skip() =>
        Unit

      case parser.If(condition,thenBody,elseBody) =>

        val nextNode = cfg.addNode(Nil)
        val (condStart,condEnd) = addStatementsToCFG(List(ExpressionStatement(condition)), cfg)

        val (thenStart,thenEnd) = addStatementsToCFG(thenBody, cfg)
        val (elseStart,elseEnd) = addStatementsToCFG(elseBody, cfg)

        cfg.addEdge(curNode, condStart, None)
        cfg.addEdge(condEnd, thenStart, Some(true))
        cfg.addEdge(condEnd, elseStart, Some(false))
        cfg.addEdge(thenEnd, nextNode, None)
        cfg.addEdge(elseEnd, nextNode, None)

        cfg.setNode(curNode,newStatements)
        newStatements = Nil
        curNode = nextNode

      case parser.While(condition,body) =>

        val nextNode = cfg.addNode(Nil)
        val (condStart,condEnd) = addStatementsToCFG(List(ExpressionStatement(condition)), cfg)
        val (bodyStart,bodyEnd) = addStatementsToCFG(body, cfg)

        cfg.addEdge(curNode, condStart, None)
        cfg.addEdge(condEnd, bodyStart, Some(true))
        cfg.addEdge(condEnd, nextNode, Some(false))
        cfg.addEdge(bodyEnd, nextNode, None)

        cfg.setNode(curNode,newStatements)
        newStatements = Nil
        curNode = nextNode

      case parser.ExpressionStatement(expr) =>

        newStatements = newStatements ::: expressionToStatement(expr) :: Nil

      case a@parser.AssignStatement(left,right) =>

        if (left.size != 1) throw TouchException("Not supported yet",statement.pos)

        if (a.isVariableDeclaration) {
          val pc = TouchProgramPoint(left.head.pos)
          val ident = left.head match {
            case parser.LocalReference(x) => VariableIdentifier(x,typeNameToType(left.head.typeName),pc)
            case parser.GlobalReference(x) => VariableIdentifier(globalReferenceIdent(x),typeNameToType(left.head.typeName),pc)
          }
          newStatements = newStatements ::: VariableDeclaration(TouchProgramPoint(statement.pos),
            Variable(pc,ident),typeNameToType(left.head.typeName),expressionToStatement(right)) :: Nil
        } else {
          newStatements = newStatements ::: Assignment(TouchProgramPoint(statement.pos),
            expressionToStatement(left.head), expressionToStatement(right) ) :: Nil
        }

      case _ => throw TouchException("Invalid statement",statement.pos)

    }

    cfg.setNode(curNode,newStatements)
    (firstNode,curNode)

  }

  private def expressionToStatement(expr:parser.Expression):Statement = {

    val pc = TouchProgramPoint(expr.pos)
    val typ = typeNameToType(expr.typeName)

    expr match {

      case parser.LocalReference(ident) =>
        Variable(pc,VariableIdentifier(ident,typ,pc))

      case parser.Access(subject,property,args) =>
        val field = FieldAccess(pc,List(expressionToStatement(subject)),property,typeNameToType(subject.typeName))
        MethodCall(pc,field,Nil,args map (expressionToStatement(_)),typ)

      case parser.Literal(t,value) =>
        NumericalConstant(pc,value,typ)

      case parser.SingletonReference(singleton) =>
        Variable(pc,VariableIdentifier(singleton,typeNameToType(expr.typeName,true),pc))

      case parser.GlobalReference(ident) =>
        Variable(pc,VariableIdentifier(globalReferenceIdent(ident),typ,pc))

      case parser.LibraryReference(ident) =>
        Variable(pc,VariableIdentifier(scriptIdent(ident),typ,pc))

    }

  }

  def scriptIdent(ident:String) = "__script_"+ident
  def globalReferenceIdent(ident:String) = "__data_"+ident

}


abstract class Named(name : String) {

  def getName : String = name

  override def equals(o : Any) : Boolean = o match {
    case x : Named => x.getName.equals(name) && x.getClass.equals(this.getClass)
    case _ => false
  }

  override def hashCode() : Int = name.hashCode()

  override def toString = name
}

case class TouchException(msg:String,pos:Position = null) extends Exception {
  override def toString:String = msg + " (Position: " + pos + ")"
}

case class TouchPackageIdentifier() extends PackageIdentifier
case class TouchMethodIdentifier(ident:String) extends MethodIdentifier {
  override def toString:String = ident
}

case class TouchClassIdentifier(name:String,typ:Type) extends Named(name) with ClassIdentifier {
  def getThisType() = typ
}

case class TouchProgramPoint(pos:Position) extends ProgramPoint {
  def getLine() = pos.line
  def getColumn() = pos.column
}

case class TouchType(name:String, isSingleton:Boolean = false, fields: List[Identifier] = List.empty[Identifier]) extends Named(name) with Type {

  var isBottom = false;
  var isTop = false;

  override def toString():String = name

  def factory() = top()
  def top() = { val res = TouchType("Top"); res.isTop = true; res }
  def bottom() = { val res = TouchType("Bottom"); res.isBottom = true; res }
  def lub(left: Type, right: Type) = if(left == right || right == bottom()) left else if (left == bottom()) right else top()
  def glb(left: Type, right: Type) = if(left == right || right == top()) left else if (left == top()) right else bottom()
  def widening(left: Type, right: Type) = lub(left,right)
  def lessEqual(r: Type) = r == this || this.isBottom || r == top()
  def isBottomExcluding(types: Set[Type]) = false

  def isObject() = (!isNumericalType())
  def isNumericalType() = (name == "Number") || (name == "Boolean")
  def isStatic() = isSingleton
  def getPossibleFields() = fields.toSet[Identifier]
  def getArrayElementsType() = None

}