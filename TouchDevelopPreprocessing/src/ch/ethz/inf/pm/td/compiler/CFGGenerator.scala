package ch.ethz.inf.pm.td.compiler

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.td._
import parser._
import parser.ExpressionStatement
import parser.MetaStatement
import parser.TableDefinition
import parser.TypeName
import semantics.{TString, RichNativeSemantics, TouchField}
import util.parsing.input.Position
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.oorepresentation.Statement
import ch.ethz.inf.pm.sample.abstractdomain.Expression
import ch.ethz.inf.pm.sample.oorepresentation.VariableDeclaration
import scala.Some
import ch.ethz.inf.pm.sample.oorepresentation.Variable
import ch.ethz.inf.pm.sample.oorepresentation.NumericalConstant
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.oorepresentation.MethodCall
import ch.ethz.inf.pm.sample.oorepresentation.EmptyStatement
import ch.ethz.inf.pm.sample.oorepresentation.Assignment
import ch.ethz.inf.pm.sample.oorepresentation.FieldAccess
import ch.ethz.inf.pm.sample.oorepresentation.VariableDeclaration
import scala.Some
import ch.ethz.inf.pm.sample.oorepresentation.Variable
import ch.ethz.inf.pm.sample.oorepresentation.NumericalConstant
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.oorepresentation.MethodCall
import ch.ethz.inf.pm.sample.oorepresentation.EmptyStatement
import ch.ethz.inf.pm.sample.oorepresentation.Assignment
import ch.ethz.inf.pm.sample.oorepresentation.FieldAccess

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
    detectUnsupportedScripts(script)
    curScriptName = scriptIdent(scriptName)
    val id = scriptIdent(scriptName)
    SystemParameters.typ = new TouchType(scriptName,true)
    val programPoint : ProgramPoint = TouchProgramPoint(script.pos)
    val typ : Type = typeNameToType(TypeName(id), true)
    val modifiers : List[Modifier] = Nil
    val name : ClassIdentifier = TouchClassIdentifier(id,typ)
    val parametricTypes : List[Type] = Nil
    val extend : List[ClassIdentifier] = Nil
    //SystemParameters.compiler.asInstanceOf[TouchCompiler].types += findTypes(script)
    val fields : List[FieldDeclaration] = findFields(script)
    val methods : List[MethodDeclaration] = findMethods(script,typ)
    val pack : PackageIdentifier = TouchPackageIdentifier()
    val inv : Expression = null
    new ClassDefinition(programPoint, typ, modifiers, name, parametricTypes, extend, fields, methods, pack, inv)
  }

  def detectUnsupportedScripts(script:parser.Script) {
    for (dec <- script.declarations) {
      dec match {
        case x:TableDefinition=>
          throw new UnsupportedLanguageFeatureException("The compiler does not support tables at the moment")
        //case x:LibraryDefinition =>
        //  throw new UnsupportedLanguageFeatureException("The compiler does not support libraries at the moment")
        case _ => ()
      }
    }
  }

  /**
   *
   * Discovers user-defined types such as Object, Table, Index, Decorator
   * and adds those definitions to the compiler
   *
   * TODO TODO TODO
   *
   * @param script The script that is searched for type declarations
   * @return A map from the types names to the type definitions
   */
//  private def findTypes(script:parser.Script):Map[String,TouchType] = {
//
//    var newTypes = Map.empty[String,TouchType]
//    var newRecordFields = Set.empty[TouchField]
//
//    def addTouchType(name:String,members:List[Member]) {
//      newTypes += (name,new TouchType(name,false,))
//    }
//
//    def addRecordsField(field:Member) {
//      newRecordFields += new TouchField(field.name,typeNameToType(field.retType))
//    }
//
//    for (dec <- script.declarations) {
//      dec match {
//        case TableDefinition(ident,typeName,keys,fields) =>
//
//          for (field <- fields) {
//            val fieldType = field.typeName.toString
//            val noFieldType = fieldType.replace("_field","")
//            if (noFieldType == "Number") addTouchType(fieldType,gNumberField(fieldType))
//            else addTouchType(fieldType,GenericTypes.gField(fieldType,noFieldType))
//          }
//
//          val fieldMembers = fields map {case parser.Parameter(x,typ) => Member(x,typ)}
//
//          typeName match {
//            case "Object" =>
//              addTouchType(ident,GenericTypes.gObject(ident,fieldMembers))
//              addTouchType(ident+"_Collection",GenericTypes.gMutableCollection(ident+"_Collection",ident))
//              addTouchType(ident+"_Constructor",List(Member("create",ident),Member("create_collection",ident+"_Collection")))
//              addRecordsField(Member(ident,ident+"_Constructor"))
//            case "Table" =>
//              addTouchType(ident,GenericTypes.gRow(ident,fieldMembers))
//              addTouchType(ident+"_Table",GenericTypes.gTable(ident+"_Table",ident))
//              addRecordsField("records",List(Member(ident+"_table",ident+"_Table")))
//            case "Index" =>
//              val keyMembers = keys map {case Parameter(x,typ) => Member(x,typ)}
//              val keyTypes = keys map {case Parameter(_,typ) => typ.toString}
//              val fieldAndKeyMembers = fieldMembers ::: keyMembers
//              addTouchType(ident,GenericTypes.gIndexMember(ident,fieldAndKeyMembers))
//              addTouchType(ident+"_Index", GenericTypes.gIndex(ident+"_Index",keyTypes, ident))
//              addRecordsField(Member(ident+"_index",ident+"_Index"))
//            case "Decorator" =>
//              if (keys.size != 1) throw TouchException("Decorators must have exactly one entry",thing.pos)
//              val decoratedType = keys.head.typeName.toString
//              val keyMembers = keys map {case Parameter(x,typ) => Member(x,typ)}
//              val fieldAndKeyMembers = fieldMembers ::: keyMembers
//              addTouchType(ident,GenericTypes.gIndexMember(ident,fieldAndKeyMembers))
//              addTouchType(decoratedType+"_Decorator", GenericTypes.gIndex(decoratedType+"_Decorator",List(decoratedType), ident))
//              addRecordsField(Member(decoratedType+"_decorator",decoratedType+"_Decorator"))
//            case _ => throw TouchException("Table type "+typeName+" not supported",thing.pos)
//
//          }
//      }
//    }
//
//    newTypes
//  }

  private def findFields(script:parser.Script):List[FieldDeclaration] = {
    (for (dec <- script.declarations) yield {
      dec match {
        case v@parser.VariableDefinition(variable,flags) =>
          val programPoint : ProgramPoint = TouchProgramPoint(v.pos)
          val modifiers : List[Modifier] = (flags flatMap {
            case ("is\\_resource",true) => Some(ResourceModifier)
            case _ => None
          }).toList
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
        case act@parser.ActionDefinition(ident,in,out,body,isEvent) =>
          val programPoint : ProgramPoint = TouchProgramPoint(act.pos)
          val modifiers : List[Modifier] = Nil
          val isPrivate = (body find {case MetaStatement("private",_) => true; case _ => false}) != None
          val name : MethodIdentifier = TouchMethodIdentifier(ident,isEvent=isEvent,isPrivate=isPrivate)
          val parametricType : List[Type] = Nil
          val arguments : List[List[VariableDeclaration]] =
            List(in map (parameterToVariableDeclaration _), out map (parameterToVariableDeclaration _))
          val returnType : Type = null // WE DO NOT USE RETURN TYPES IN TOUCHDEVELOP. SECOND ELEMENT OF PARAM REPR. OUT PARAMS
          val newBody : ControlFlowGraph = new ControlFlowGraph(programPoint)
          addStatementsToCFG(body,newBody)
          val preCond : Statement = null
          val postCond : Statement = null
          Some(new CallableMethodDeclaration(programPoint,ownerType,modifiers,name,parametricType,arguments,returnType,
            newBody,preCond,postCond))
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

  private def typeNameToType(typeName:parser.TypeName, isSingleton:Boolean = false):TouchType = {
    if (!typeName.ident.startsWith("__script_")) {
      SystemParameters.compiler.asInstanceOf[TouchCompiler].types.get(typeName.ident) match {
        case Some(x) => x
        case None => throw new TouchException("Could not find type "+typeName)
      }
    } else new TouchType(typeName.ident,isSingleton)
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
        cfg.addEdge(bodyEnd, condStart, None)

        cfg.setNode(curNode,newStatements)
        newStatements = Nil
        curNode = nextNode

      case parser.ExpressionStatement(expr) =>

        newStatements = newStatements ::: expressionToStatement(expr) :: Nil

      case a@parser.AssignStatement(left,right) =>

        if (left.size != 1) {

          right match {
            case parser.Access(parser.SingletonReference("code"),prop,args) =>
              val pc = TouchProgramPoint(left.head.pos)
              val typ = TouchTuple(left map {case x:parser.LValue => typeNameToType(x.typeName)})
              val ident = VariableIdentifier(tupleIdent(pc),typ,pc)
              newStatements = newStatements ::: VariableDeclaration(TouchProgramPoint(statement.pos),
                Variable(pc,ident),typ,expressionToStatement(right)) :: Nil
              var x = 0
              for(l <- left) {
                x = x + 1
                newStatements = newStatements ::: Assignment(pc,
                  expressionToStatement(l), FieldAccess(pc, List(Variable(pc,ident)),"_"+x,typeNameToType(l.typeName)) ) :: Nil
              }
            case _ =>  throw TouchException("Not allowed",statement.pos)
          }

        } else {

          if (a.isVariableDeclaration) {
            val pc = TouchProgramPoint(left.head.pos)
            val ident = left.head match {
              case parser.LocalReference(x) =>
                VariableIdentifier(x,typeNameToType(left.head.typeName),pc)
              case parser.GlobalReference(x) =>
                VariableIdentifier(globalReferenceIdent(x),typeNameToType(left.head.typeName),pc)
            }
            newStatements = newStatements ::: VariableDeclaration(TouchProgramPoint(statement.pos),
              Variable(pc,ident),typeNameToType(left.head.typeName),expressionToStatement(right)) :: Nil
          } else {
            newStatements = newStatements ::: Assignment(TouchProgramPoint(statement.pos),
              expressionToStatement(left.head), expressionToStatement(right) ) :: Nil
          }

        }

      case _ => throw TouchException("Invalid statement",statement.pos)

    }

    cfg.setNode(curNode,newStatements)
    (firstNode,curNode)

  }

  private def expressionToStatement(expr:parser.Expression):Statement = {

    val pc = TouchProgramPoint(expr.pos)
    if (expr == parser.SingletonReference("skip")) return EmptyStatement(pc)

    val typ = typeNameToType(expr.typeName)

    expr match {

      case parser.LocalReference(ident) =>
        Variable(pc,VariableIdentifier(ident,typ,pc))

      case parser.Access(subject,property,args) =>
        val field = FieldAccess(pc,List(expressionToStatement(subject)),property,typeNameToType(subject.typeName))
        MethodCall(pc,field,Nil,args map (expressionToStatement(_)),typ)

      case parser.Literal(t,value) =>
        if (t.ident == "Number" || t.ident == "Boolean") {
          NumericalConstant(pc,value,typ)
        } else if (t.ident == "String") {
          StringConstant(pc,value)
        } else throw new TouchException("Literals with type "+t.ident+" do not exist")

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
  def tupleIdent(a:ProgramPoint) = "__tuple_"+a.getLine()+"_"+a.getColumn()

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

case class UnsupportedLanguageFeatureException(msg:String) extends Exception {
  override def toString:String = msg
}

case class TouchPackageIdentifier() extends PackageIdentifier


case class TouchMethodIdentifier(ident:String,isEvent:Boolean,isPrivate:Boolean) extends MethodIdentifier {
  override def toString:String = ident
}

case class TouchClassIdentifier(name:String,typ:Type) extends Named(name) with ClassIdentifier {
  def getThisType() = typ
}

case class TouchProgramPoint(pos:Position) extends ProgramPoint {
  def getLine() = pos.line
  def getColumn() = pos.column
  override def toString = "{"+pos.line+","+pos.column+"}"
}

case class TouchSingletonProgramPoint(name:String) extends ProgramPoint {
  def getLine() = 0
  def getColumn() = 0
  override def toString = name
}

case class TouchInitializationProgramPoint(name:String) extends ProgramPoint {
  def getLine() = 0
  def getColumn() = 0
  override def toString = name
}

/**
 *
 * The deepening program point is used to generate more precise program point heap identifiers in case of
 * deep allocation of objects.
 *
 * If for example, at one point we have to create an object (Picture) with its fields (e.g. location) at the same time,
 * we don't want the Picture and the Location to be represented by the same heap identifier. For this purpose, we
 * "deepen" the program point by extending it with a sequence of strings. The picture location of the Picture allocated
 * at line 7, column 9 would then get the Heap Identifier (7,9)->location,
 *
 * To make sure this terminates, repeating sequences will get mapped to the corresponding prefix with only unique
 * elements. For example, {7,9}->album->cover_picture->album will get mapped to {7,9}->album
 *
 */
object DeepeningProgramPoint {
  def apply(pp:ProgramPoint,name:String):(DeepeningProgramPoint,Boolean) = {
    pp match {
      case DeepeningProgramPoint(realPP,path) =>
        if (path.contains(name)) (DeepeningProgramPoint(realPP,path.takeWhile(!_.equals(name)) ::: (name :: Nil)),true)
        else (DeepeningProgramPoint(realPP,path ::: (name :: Nil)),false)
      case _ => (DeepeningProgramPoint(pp,name :: Nil),false)
    }
  }
}

case class DeepeningProgramPoint(pp:ProgramPoint,path:List[String]) extends ProgramPoint {
  def getLine() = pp.getLine()
  def getColumn() = pp.getColumn()
  override def toString = pp+"("+path.mkString(",")+")"
}

class TouchType(name:String, val isSingleton:Boolean = false, fields: List[Identifier] = List.empty[Identifier]) extends Named(name) with Type {

  var isBottom = false;
  var isTop = false;

  override def toString():String = name

  def factory() = top()
  def top() = { val res = new TouchType("Top"); res.isTop = true; res }
  def bottom() = { val res = new TouchType("Bottom"); res.isBottom = true; res }
  def lub(left: Type, right: Type) = if(left == right || right == bottom()) left else if (left == bottom()) right else top()
  def glb(left: Type, right: Type) = if(left == right || right == top()) left else if (left == top()) right else bottom()
  def widening(left: Type, right: Type) = lub(left,right)
  def lessEqual(r: Type) = r == this || this.isBottom || r == top()
  def isBottomExcluding(types: Set[Type]) = false

  def isObject() = (!isNumericalType())
  def isNumericalType() = (name == "Number") || (name == "Boolean")
  def isStatic() = isSingleton
  def getPossibleFields() = fields.toSet[Identifier]
  def getPossibleFieldsSorted() = fields
  def getArrayElementsType() = None

}

object TouchTuple {
  def apply(types:List[TouchType]):TouchTuple = {
    var x = 0
    val fields = for (typ <- types) yield {
      x = x + 1
      new TouchField("_"+x,typ)
    }
    TouchTuple(types.mkString(","),fields)
  }
}

case class TouchTuple(name:String, fields:List[TouchField]) extends TouchType(name,false,fields)

case class TouchCollection(name:String,keyType:String,valueType:String, fields: List[Identifier] = List.empty[Identifier]) extends TouchType(name,false,fields) {

  def getKeyType = SystemParameters.compiler.asInstanceOf[TouchCompiler].types(keyType)
  def getValueType = SystemParameters.compiler.asInstanceOf[TouchCompiler].types(valueType)

}


/**
 * The resource modifier marks any field of a class that represents a preloaded artwork,
 * mostly images loaded from URLs.
 */
case object ResourceModifier extends Modifier


/**
 * This class represents a string literal in TouchDevelop
 */
case class StringConstant(pp : ProgramPoint, value : String) extends Statement(pp)  {

  override def forwardSemantics[S <: State[S]](state : S) : S = {
    RichNativeSemantics.New[S](TString.typ,Map(TString.field_count.asInstanceOf[Identifier] -> RichNativeSemantics.toRichExpression(value.length)))(state,pp)
  }

  override def backwardSemantics[S <: State[S]](state : S) : S = state

  override def toString() : String = value

  override def toSingleLineString() : String = toString()

}

