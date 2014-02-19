package ch.ethz.inf.pm.td.compiler

import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.td._
import ch.ethz.inf.pm.td.parser._
import semantics._
import util.parsing.input.Position
import ch.ethz.inf.pm.sample.{oorepresentation, SystemParameters}
import ch.ethz.inf.pm.sample.oorepresentation.Statement
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.td.parser.TableDefinition
import ch.ethz.inf.pm.td.parser.WhereStatement
import ch.ethz.inf.pm.td.parser.InlineAction
import ch.ethz.inf.pm.sample.oorepresentation.VariableDeclaration
import ch.ethz.inf.pm.td.parser.ExpressionStatement
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.oorepresentation.MethodCall
import ch.ethz.inf.pm.td.parser.LibraryDefinition
import ch.ethz.inf.pm.sample.abstractdomain.Identifier
import ch.ethz.inf.pm.td.parser.MetaStatement
import ch.ethz.inf.pm.td.parser.Box
import ch.ethz.inf.pm.td.parser.Parameter
import ch.ethz.inf.pm.sample.oorepresentation.Variable
import ch.ethz.inf.pm.sample.oorepresentation.ConstantStatement
import ch.ethz.inf.pm.sample.oorepresentation.EmptyStatement
import ch.ethz.inf.pm.sample.oorepresentation.FieldAccess
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.sample.abstractdomain.Expression
import ch.ethz.inf.pm.td.typecheck.Typer

/**
 *
 * Lucas Brutschy
 * Date: 8/22/12
 * Time: 4:21 PM
 *
 */

object CFGGenerator {
  def handlerIdent(ident:String) = "__handler_"+ident
  def isHandlerIdent(ident:String) = ident.startsWith("__handler_")
  def globalReferenceIdent(ident:String) = "__data_"+ident
  def isGlobalReferenceIdent(ident:String) = ident.startsWith("__data_")
  def paramIdent(ident:String) = "__param_"+ident
  def isParamIdent(ident:String) = ident.startsWith("__param_")
  def libraryIdent(ident:String) = "♻"+ident
  def isLibraryIdent(ident:String) = ident.startsWith("♻") && ident.length() > 1
  def getLibraryName(ident:String) = ident.substring(1)
  def returnIdent(ident:String) = "__returned_"+ident
  def isReturnIdent(ident:String) = ident.startsWith("__returned_")
  def isNonDetIdent(ident:String) = ident.startsWith("__nondet")
}


class CFGGenerator(compiler: TouchCompiler) {
  import CFGGenerator._

  private var curPubID:String = ""
  private var curScriptName:String = ""

  def process(script:parser.Script, pubID:String, libDef:Option[LibraryDefinition] = None):ClassDefinition = {
    //detectUnsupportedScripts(script)
    curPubID = pubID
    libDef match {
      case Some(LibraryDefinition(name,_,_,_)) => curScriptName = libraryIdent(name)
      case None => curScriptName = libraryIdent(pubID)
    }
    val programPoint : ProgramPoint = mkTouchProgramPoint(script)
    val typ : Type = typeNameToType(TypeName(curScriptName), true)
    SystemParameters.typ = typ

    findTypes(script)

    val modifiers : List[Modifier] = Nil
    val name : ClassIdentifier = TouchClassIdentifier(curScriptName,typ)
    val parametricTypes : List[Type] = Nil
    val extend : List[ClassIdentifier] = Nil
    val pack : PackageIdentifier = TouchPackageIdentifier()
    val inv : Expression = null
    val classDef = new ClassDefinition(programPoint, typ, modifiers, name, parametricTypes, extend, null, null, pack, inv)
    val fields : List[FieldDeclaration] = findFields(script, classDef)
    val methods : List[MethodDeclaration] = findMethods(script,typ, classDef)
    classDef.fields = fields
    classDef.methods = methods
    classDef
  }

  def mkTouchProgramPoint(element:IdPositional) = {
    val pos = element.getPositionAsString
    TouchProgramPoint(curPubID,pos)
  }

  /**
   *
   * Discovers user-defined types such as Object, Table, Index, Decorator
   * and adds those definitions to the compiler
   *
   * @param script The script that is searched for type declarations
   */
  private def findTypes(script:parser.Script) {

    def addTouchType(semantics:AAny) {
      compiler.userTypes += semantics.getTypeName -> semantics
    }

    def addRecordsField(field:TouchField) {
      SRecords.typ = new TouchType(SRecords.typName,isSingleton = true, fields = SRecords.typ.possibleFields.toList ::: List(field))
    }

    def createFieldMembers(fields:List[Parameter]): List[(TouchField,TouchField)] = {
      for (field <- fields) yield {
        val inp = field.typeName.toString
        val (fieldTypeName, noFieldTypeName) =
          if(inp.matches(""" field$""")) {
            val (part1,check) = inp.splitAt(inp.size-7)
            if (check != " field") throw TouchException("Expected field here")
            (part1+" field",part1)
          } else {
            (inp+" field",inp)
          }
        val valueField = new TouchField("*value",noFieldTypeName)
        val fieldType = new TouchType(fieldTypeName,fields=List(valueField))

        if (noFieldTypeName.equals("Number"))
          addTouchType(new ANumberField(fieldType,valueField))
        else
          addTouchType(new AField(fieldType,valueField))

        (new TouchField(field.ident,fieldTypeName,NewInitializer),valueField)
      }
    }

    for (dec <- script.declarations) {
      dec match {
        case thing@TableDefinition(ident,typeName,keys,fields) =>


          typeName match {
            case "object" =>

              val objectTyp = new TouchType(ident,fields = createFieldMembers(fields) map (_._1))
              val collectionTyp = new TouchCollection(ident+" Collection",TNumber.typName,ident)
              val constructorTyp = new TouchType(ident+" Constructor")

              addTouchType(new AObject(objectTyp))
              addTouchType(new AObjectCollection(collectionTyp,objectTyp))
              addTouchType(new AObjectConstructor(constructorTyp,objectTyp,collectionTyp))

              addRecordsField(new TouchField(ident,constructorTyp.name))

            case "table" =>

              val rowTypName = ident
              val tableTypeName = ident+" Table"

              // An auxiliary field that stores a link from the row to the table, to implement row.delete_row
              val tableField = new TouchField("*table",tableTypeName)

              val rowTyp = new TouchType(ident,fields = (createFieldMembers(fields) map (_._1)) ::: List(tableField))
              val tableTyp = new TouchCollection(tableTypeName,TNumber.typName,rowTypName)

              addTouchType(new ARow(rowTyp,tableField))
              addTouchType(new ATable(tableTyp,rowTyp,tableField))

              addRecordsField(new TouchField(ident+" table",tableTyp.name))

            case "index" =>

              val keyMembers = keys map {case Parameter(x,typ) => new TouchField(x,typ.ident)}
              val fieldMembers = createFieldMembers(fields)

              val indexMemberType = new TouchType(ident,fields = (fieldMembers map (_._1))  ::: keyMembers)
              val keyTypes = keyMembers map (_.getType.asInstanceOf[TouchType])

              addTouchType(new AIndexMember(indexMemberType,fieldMembers))
              val indexType =
                if (keyTypes.size > 0) {
                  val ty = new TouchCollection(ident+" Index",TNumber.typName,indexMemberType.name)
                  addTouchType(new AIndex(ty,keyTypes,indexMemberType))
                  ty
                } else {
                  val ty = new TouchType(ident+" Index",fields = List(new TouchField("singleton",indexMemberType.name)))
                  addTouchType(new ASingletonIndex(ty,indexMemberType))
                  ty
                }

              addRecordsField(new TouchField(ident+" index",indexType.name))

            case "decorator" =>

              if (keys.size != 1) throw TouchException("Decorators must have exactly one entry "+thing.getPositionDescription)

              val keyMembers = keys map {case Parameter(x,typ) => new TouchField(x,typ.ident)}
              val fieldMembers = createFieldMembers(fields)

              val decoratedType = keyMembers.head.getType.asInstanceOf[TouchType]
              val decorationType = new TouchType(ident,fields = (fieldMembers map (_._1)) ::: keyMembers)
              val decoratorType = new TouchCollection(decoratedType+" Decorator",decoratedType.name,decorationType.name)

              addTouchType(new AIndexMember(decorationType,fieldMembers))
              addTouchType(new AIndex(decoratorType,List(decoratedType),decorationType))

              addRecordsField(new TouchField(decoratedType+" decorator",decoratorType.name))

            case _ => throw TouchException("Table type "+typeName+" not supported "+thing.getPositionDescription)

          }

        case _ => ()
      }
    }
  }

  private def findFields(script:parser.Script, currentClassDef: ClassDefinition):List[FieldDeclaration] = {
    (for (dec <- script.declarations) yield {
      dec match {
        case v@parser.VariableDefinition(variable,flags) =>
          val programPoint : ProgramPoint = mkTouchProgramPoint(v)
          val modifiers : List[Modifier] = (flags flatMap {
            case ("is_resource","true") => Some(ResourceModifier)
            case ("readonly","true") => Some(ReadOnlyModifier)
            case _ => None
          }).toList
          val name: Variable = parameterToVariable(variable)
          val typ : Type = typeNameToType(variable.typeName)
          Some(new FieldDeclaration(programPoint, modifiers, name, typ))
        case _ => None
      }
    }).flatten
  }

  private def findMethods(script:parser.Script,ownerType:Type, currentClassDef: ClassDefinition):List[MethodDeclaration] = {
    (for (dec <- script.declarations) yield {
      dec match {
        case act@parser.ActionDefinition(ident,in,out,body,isEvent,isPriv) =>
          val programPoint : ProgramPoint = mkTouchProgramPoint(act)
          val scope : ScopeIdentifier = ProgramPointScopeIdentifier(programPoint)
          val modifiers : List[Modifier] = Nil
          val isPrivate = isPriv || ((body find {case MetaStatement("private",_) => true; case _ => false}) != None)
          val name : MethodIdentifier = TouchMethodIdentifier(ident,isEvent=isEvent,isPrivate=isPrivate)
          val parametricType : List[Type] = Nil
          val arguments : List[List[VariableDeclaration]] =
            List(in map (parameterToVariableDeclaration(_,scope)), out map (parameterToVariableDeclaration(_,scope)))
          val returnType : Type = null // WE DO NOT USE RETURN TYPES IN TOUCHDEVELOP. SECOND ELEMENT OF PARAM REPR. OUT PARAMS
          val newBody : ControlFlowGraph = new ControlFlowGraph(programPoint)
          val (_,_,handlers) = addStatementsToCFG(body,newBody,scope, currentClassDef)
          val preCond : Statement = null
          val postCond : Statement = null
          handlers ::: List(new MethodDeclaration(programPoint,ownerType,modifiers,name,parametricType,arguments,
            returnType,newBody,preCond,postCond, currentClassDef))
        case act@parser.PageDefinition(ident,in,out,initBody,displayBody,isPriv) =>
          val programPoint : ProgramPoint = mkTouchProgramPoint(act)
          val scope : ScopeIdentifier = ProgramPointScopeIdentifier(programPoint)
          val modifiers : List[Modifier] = Nil
          val name : MethodIdentifier = TouchMethodIdentifier(ident,isEvent=false,isPrivate=isPriv)
          val parametricType : List[Type] = Nil
          val arguments : List[List[VariableDeclaration]] =
            List(in map (parameterToVariableDeclaration(_,scope)), out map (parameterToVariableDeclaration(_,scope)))
          val returnType : Type = null // WE DO NOT USE RETURN TYPES IN TOUCHDEVELOP. SECOND ELEMENT OF PARAM REPR. OUT PARAMS
          val newBody : ControlFlowGraph = new ControlFlowGraph(programPoint)
          val (_,_,handlers) = addStatementsToCFG(initBody ::: displayBody,newBody,scope, currentClassDef)
          val preCond : Statement = null
          val postCond : Statement = null
          handlers ::: List(new MethodDeclaration(programPoint,ownerType,modifiers,name,parametricType,arguments,
            returnType,newBody,preCond,postCond, currentClassDef))
        case _ => Nil
      }
    }).flatten
  }

  private def parameterToVariableDeclaration(parameter: parser.Parameter, scope: ScopeIdentifier = EmptyScopeIdentifier): VariableDeclaration = {
    val programPoint : ProgramPoint = mkTouchProgramPoint(parameter)
    val variable : Variable = parameterToVariable(parameter,scope)
    val typ : Type = typeNameToType(parameter.typeName)
    VariableDeclaration(programPoint, variable, typ)
  }

  private def parameterToVariable(parameter: parser.Parameter, scope: ScopeIdentifier = EmptyScopeIdentifier): Variable = {
    val programPoint : ProgramPoint = mkTouchProgramPoint(parameter)
    val id : VariableIdentifier = parameterToVariableIdentifier(parameter,scope)
    Variable(programPoint,id)
  }

  private def parameterToVariableIdentifier(parameter: parser.Parameter, scope: ScopeIdentifier = EmptyScopeIdentifier): VariableIdentifier = {
    val name : String = parameter.ident
    val typ : Type = typeNameToType(parameter.typeName)
    val programPoint : ProgramPoint = mkTouchProgramPoint(parameter)
    VariableIdentifier(name, scope)(typ, programPoint)
  }

  private def typeNameToType(typeName:parser.TypeName, isSingleton:Boolean = false):TouchType = {
    if (!isLibraryIdent(typeName.ident)) {
      compiler.getSemantics(typeName.ident).getTyp
    } else new TouchType(typeName.ident,isSingleton)
  }

  private def addStatementsToCFG(statements:List[parser.Statement], cfg:ControlFlowGraph, scope:ScopeIdentifier,
                                  currentClassDef: ClassDefinition):(Int,Int,List[MethodDeclaration]) = {

    val firstNode = cfg.addNode(Nil)
    var newStatements:List[Statement] = Nil
    var newHandlers:List[MethodDeclaration] = Nil
    var curNode = firstNode

    for (statement <- statements) statement match {

      case parser.MetaStatement(_,_) =>
        Unit

      case parser.Skip() =>
        Unit

      case parser.If(condition,thenBody,elseBody) =>

        val nextNode = cfg.addNode(Nil)
        val (condStart,condEnd,handlersCond) = addStatementsToCFG(List(ExpressionStatement(condition)), cfg, scope, currentClassDef)

        val (thenStart,thenEnd,handlersThen) = addStatementsToCFG(thenBody, cfg, scope, currentClassDef)
        val (elseStart,elseEnd,handlersElse) = addStatementsToCFG(elseBody, cfg, scope, currentClassDef)

        cfg.addEdge(curNode, condStart, None)
        cfg.addEdge(condEnd, thenStart, Some(true))
        cfg.addEdge(condEnd, elseStart, Some(false))
        cfg.addEdge(thenEnd, nextNode, None)
        cfg.addEdge(elseEnd, nextNode, None)

        cfg.setNode(curNode,newStatements)
        newStatements = Nil
        newHandlers = newHandlers ::: handlersCond ::: handlersThen ::: handlersElse
        curNode = nextNode

      case parser.While(condition,body) =>

        val nextNode = cfg.addNode(Nil)
        val (condStart,condEnd,handlersCond) = addStatementsToCFG(List(ExpressionStatement(condition)), cfg, scope, currentClassDef)
        val (bodyStart,bodyEnd,handlersBody) = addStatementsToCFG(body, cfg, scope, currentClassDef)

        cfg.addEdge(curNode, condStart, None)
        cfg.addEdge(condEnd, bodyStart, Some(true))
        cfg.addEdge(condEnd, nextNode, Some(false))
        cfg.addEdge(bodyEnd, condStart, None)

        cfg.setNode(curNode,newStatements)
        newStatements = Nil
        newHandlers = newHandlers ::: handlersCond ::: handlersBody
        curNode = nextNode

      case parser.ExpressionStatement(expr) =>

        newStatements = newStatements ::: expressionToStatement(expr,scope) :: Nil

      case b@Box(body) =>

        // TODO: what else?
        val nextNode = cfg.addNode(Nil)
        val (bodyStart,bodyEnd,handlersBody) = addStatementsToCFG(body,cfg, scope, currentClassDef)

        cfg.addEdge(curNode, bodyStart, None)
        cfg.addEdge(bodyEnd, nextNode, None)

        cfg.setNode(curNode,newStatements)
        newStatements = Nil
        newHandlers = newHandlers ::: handlersBody
        curNode = nextNode

      case w@WhereStatement(expr,handlerDefs:List[InlineAction]) =>

        val handlerSet =
          for (InlineAction(handlerName,inParameters,_,_) <- handlerDefs) yield {
            ( handlerName, handlerIdent(handlerName+mkTouchProgramPoint(w)), Typer.inParametersToActionType(inParameters) )
          }

        val handlers = (for (InlineAction(handlerName,inParameters,outParameters,body) <- handlerDefs) yield {
          val handlerMethodName = handlerIdent(handlerName+mkTouchProgramPoint(w))
          val programPoint : ProgramPoint = mkTouchProgramPoint(w)
          val modifiers : List[Modifier] = Nil
          val name : MethodIdentifier = TouchMethodIdentifier(handlerMethodName,isEvent = true,isPrivate = true)
          val parametricType : List[Type] = Nil
          val arguments : List[List[VariableDeclaration]] =
            List(inParameters map (parameterToVariableDeclaration(_,scope)), outParameters map (parameterToVariableDeclaration(_,scope)))
          val returnType : Type = null
          val newBody : ControlFlowGraph = new ControlFlowGraph(programPoint)
          val (_,_,subHandlers) = addStatementsToCFG(body,newBody,scope, currentClassDef)
          val preCond : Statement = null
          val postCond : Statement = null
          subHandlers ::: List(new MethodDeclaration(programPoint,currentClassDef.typ,modifiers,name,parametricType,
            arguments,returnType,newBody,preCond,postCond, currentClassDef))
        }).flatten

        def ty (typ:String,expr:parser.Expression):parser.Expression = {
          expr.typeName = TypeName(typ)
          expr
        }

        // Create a statement that creates the handler object and assigns the handler variable
        val handlerCreationStatements = handlerSet map {
          case (variableName:String, actionName:String, handlerType:TypeName) =>
            expressionToStatement(
              ty("Nothing",parser.Access(
                ty(handlerType.toString,parser.LocalReference(variableName)),
                Identifier(":="),
                List(
                 ty(handlerType.toString,parser.Access(
                   ty("Helpers",parser.SingletonReference("helpers","Helpers")),
                   Identifier("create "+handlerType.ident.toLowerCase+" "+actionName),
                   Nil
                 ))
                )
              )),
              scope
            )
        }

        newStatements = newStatements ::: handlerCreationStatements ::: List(expressionToStatement(expr,scope))
        newHandlers = newHandlers ::: handlers

      case _ =>
        throw TouchException("Invalid statement",statement.pos)

    }

    cfg.setNode(curNode,newStatements)
    (firstNode,curNode,newHandlers)

  }

  private def expressionToStatement(expr:parser.Expression, scope:ScopeIdentifier):Statement = {

    val pc = mkTouchProgramPoint(expr)
    if (expr == parser.SingletonReference("skip","Nothing")) return EmptyStatement(pc)
    if (expr == parser.SingletonReference("skip","Skip")) return EmptyStatement(pc)

    val typ = typeNameToType(expr.typeName)

    expr match {

      case parser.LocalReference(ident) =>
        Variable(pc, VariableIdentifier(ident, scope)(typ, pc))

      case parser.Access(subject,property,args) =>
        val field = FieldAccess(mkTouchProgramPoint(property), expressionToStatement(subject, scope), property.ident, typeNameToType(subject.typeName))
        MethodCall(mkTouchProgramPoint(property),field,Nil,args map (expressionToStatement(_,scope)),typ)

      case parser.Literal(t,value) =>
        if (t.ident == "Number" || t.ident == "Boolean" || t.ident == "String" || t.ident == "Handler") {
          ConstantStatement(pc,value,typ)
        } else throw new TouchException("Literals with type "+t.ident+" do not exist")

      case parser.SingletonReference(singleton, typ) =>
        Variable(pc, VariableIdentifier(singleton)(typeNameToType(expr.typeName, isSingleton = true), pc))
    }

  }
}


abstract class Named {
  def name: String

  override def equals(o: Any): Boolean = o match {
    case x: Named => x.name == name && x.getClass == this.getClass
    case _ => false
  }

  override def hashCode: Int = name.hashCode()

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

case class TouchClassIdentifier(name: String, typ: Type) extends Named with ClassIdentifier {
  def getThisType() = typ
}

case class TouchProgramPoint(scriptID:String, pos:String) extends ProgramPoint {
  def getScriptID:String = scriptID
  override def toString = "PP("+getScriptID+":"+pos+")"
  override def description = "in script "+scriptID+" at node "+pos
}

case class TouchSingletonProgramPoint(name:String) extends ProgramPoint {
  override def toString = "Init("+name+")"
  override def description = "at initialization of singleton "+name
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
  override def toString = pp+"("+path.mkString(",")+")"
  override def description = pp+" at initialization path "+path.mkString(",")
}

class TouchType(
    val name: String,
    val isSingleton: Boolean = false,
    val isImmutable: Boolean = false,
    fields: List[Identifier] = List.empty[Identifier])
  extends Named with Type {

  var isBottom = false
  var isTop = false

  def factory() = top()
  def top() = { val res = new TouchType("Top"); res.isTop = true; res }
  def bottom() = { val res = new TouchType("Bottom"); res.isBottom = true; res }

  def lub(other: oorepresentation.Type): oorepresentation.Type = {
    if (other == null) return this
    val other_ = other.asInstanceOf[TouchType]
    if (isTop || other_.isTop) return top()
    if (isBottom) return other_
    if (other_.isBottom) return this
    if (!equals(other_)) top()
    else this
  }

  def glb(other: oorepresentation.Type): oorepresentation.Type = {
    if (other == null) return this
    val other_ = other.asInstanceOf[TouchType]
    if (isBottom || other_.isBottom) return bottom()
    if (isTop) return other_
    if (other_.isTop) return this
    if (!equals(other_)) bottom()
    else this
  }

  def widening(other: Type) = lub(other)
  
  def lessEqual(other: Type) = other == this || this.isBottom || other == top()

  def isBottomExcluding(types: Set[Type]) = isBottom || types.contains(this)

  def isObject = !isNumericalType && !isStringType
  def isBooleanType = name == "Boolean"
  def isNumericalType = (name == "Number") || (name == "Boolean")
  def isFloatingPointType = name == "Number" || name == "Boolean" // TODO: Booleans should not be floating points
  def isStringType = name == "String"
  def isStatic = isSingleton
  def possibleFields = fields.toSet[Identifier]
  def possibleTouchFields = fields.toSet[Identifier] map (_.asInstanceOf[TouchField])
  def arrayElementsType = None

}

case class TouchCollection(
    override val name: String,
    keyType: String,
    valueType: String,
    fields: List[Identifier] = List.empty[Identifier],
    immutableCollection: Boolean = false)
  extends TouchType(name, false, immutableCollection, fields) {

  def getKeyType = SystemParameters.compiler.asInstanceOf[TouchCompiler].getSemantics(keyType).getTyp
  def getValueType = SystemParameters.compiler.asInstanceOf[TouchCompiler].getSemantics(valueType).getTyp

}


/**
 * The resource modifier marks any field of a class that represents a preloaded artwork,
 * mostly images loaded from URLs.
 */
case object ResourceModifier extends Modifier
case object ReadOnlyModifier extends Modifier
