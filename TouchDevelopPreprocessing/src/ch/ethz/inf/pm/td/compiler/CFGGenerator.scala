package ch.ethz.inf.pm.td.compiler

import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.td._
import ch.ethz.inf.pm.td.parser._
import semantics._
import util.parsing.input.Position
import ch.ethz.inf.pm.sample.{oorepresentation, SystemParameters}
import ch.ethz.inf.pm.sample.oorepresentation.Statement
import ch.ethz.inf.pm.sample.abstractdomain.Expression
import ch.ethz.inf.pm.td.parser.Parameter
import ch.ethz.inf.pm.td.parser.TableDefinition
import ch.ethz.inf.pm.td.semantics.NewInitializer
import ch.ethz.inf.pm.td.parser.WhereStatement
import ch.ethz.inf.pm.sample.oorepresentation.VariableDeclaration
import scala.Some
import ch.ethz.inf.pm.sample.oorepresentation.Variable
import ch.ethz.inf.pm.sample.oorepresentation.ConstantStatement
import ch.ethz.inf.pm.td.parser.ExpressionStatement
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.oorepresentation.MethodCall
import ch.ethz.inf.pm.sample.oorepresentation.EmptyStatement
import ch.ethz.inf.pm.td.parser.LibraryDefinition
import ch.ethz.inf.pm.sample.oorepresentation.Assignment
import ch.ethz.inf.pm.sample.abstractdomain.Identifier
import ch.ethz.inf.pm.sample.oorepresentation.FieldAccess
import ch.ethz.inf.pm.td.parser.MetaStatement
import ch.ethz.inf.pm.td.parser.Box
import ch.ethz.inf.pm.td.parser.TypeName

/**
 *
 * Lucas Brutschy
 * Date: 8/22/12
 * Time: 4:21 PM
 *
 */
object CFGGenerator {

  var curPubID:String = ""
  var curScriptName:String = ""

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
    val fields : List[FieldDeclaration] = findFields(script)
    val methods : List[MethodDeclaration] = findMethods(script,typ)
    val pack : PackageIdentifier = TouchPackageIdentifier()
    val inv : Expression = null
    new ClassDefinition(programPoint, typ, modifiers, name, parametricTypes, extend, fields, methods, pack, inv)
  }

  def mkTouchProgramPoint(element:IdPositional) = {
    TouchProgramPoint(curPubID,element.getPositionAsString)
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
      SystemParameters.compiler.asInstanceOf[TouchCompiler].userTypes =
        SystemParameters.compiler.asInstanceOf[TouchCompiler].userTypes + ((semantics.getTypeName,semantics))
    }

    def addRecordsField(field:TouchField) {
      SRecords.typ = new TouchType(SRecords.typName,isSingleton = true, fields = SRecords.typ.getPossibleFields().toList ::: List(field))
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
        val noFieldType = typeNameToType(TypeName(noFieldTypeName))
        val valueField = new TouchField("*value",noFieldType)
        val fieldType = new TouchType(fieldTypeName,fields=List(valueField))

        if (noFieldType.getName().equals("Number"))
          addTouchType(new ANumberField(fieldType,valueField))
        else
          addTouchType(new AField(fieldType,valueField))

        (new TouchField(field.ident,fieldType,NewInitializer()),valueField)
      }
    }

    for (dec <- script.declarations) {
      dec match {
        case thing@TableDefinition(ident,typeName,keys,fields) =>


          typeName match {
            case "object" =>

              val objectTyp = new TouchType(ident,fields = (createFieldMembers(fields) map (_._1)))
              val collectionTyp = new TouchCollection(ident+" Collection",TNumber.typName,ident)
              val constructorTyp = new TouchType(ident+" Constructor")

              addTouchType(new AObject(objectTyp))
              addTouchType(new AObjectCollection(collectionTyp,objectTyp))
              addTouchType(new AObjectConstructor(constructorTyp,objectTyp,collectionTyp))

              addRecordsField(new TouchField(ident,constructorTyp))

            case "table" =>

              val rowTyp = new TouchType(ident,fields = (createFieldMembers(fields) map (_._1)))
              val tableTyp = new TouchCollection(ident+" Table",TNumber.typName,rowTyp.getName())

              addTouchType(new ARow(rowTyp))
              addTouchType(new ATable(tableTyp,rowTyp))

              addRecordsField(new TouchField(ident+" table",tableTyp))

            case "index" =>

              val keyMembers = keys map {case Parameter(x,typ) => new TouchField(x,typeNameToType(typ))}
              val fieldMembers = createFieldMembers(fields)

              val indexMemberType = new TouchType(ident,fields = (fieldMembers map (_._1))  ::: keyMembers)
              val keyTypes = keyMembers map (_.getType().asInstanceOf[TouchType])

              addTouchType(new AIndexMember(indexMemberType,fieldMembers))
              val indexType =
                if (keyTypes.size > 0) {
                  val ty = new TouchCollection(ident+" Index",TNumber.typName,indexMemberType.getName())
                  addTouchType(new AIndex(ty,keyTypes,indexMemberType))
                  ty
                } else {
                  val ty = new TouchType(ident+" Index",fields = List(new TouchField("singleton",indexMemberType)))
                  addTouchType(new ASingletonIndex(ty,indexMemberType))
                  ty
                }

              addRecordsField(new TouchField(ident+" index",indexType))

            case "decorator" =>

              if (keys.size != 1) throw TouchException("Decorators must have exactly one entry "+thing.getPositionDescription)

              val keyMembers = keys map {case Parameter(x,typ) => new TouchField(x,typeNameToType(typ))}
              val fieldMembers = createFieldMembers(fields)

              val decoratedType = keyMembers.head.getType().asInstanceOf[TouchType]
              val decorationType = new TouchType(ident,fields = (fieldMembers map (_._1)) ::: keyMembers)
              val decoratorType = new TouchCollection(decoratedType+" Decorator",decoratedType.getName(),decorationType.getName())

              addTouchType(new AIndexMember(decorationType,fieldMembers))
              addTouchType(new AIndex(decoratorType,List(decoratedType),decorationType))

              addRecordsField(new TouchField(decoratedType+" decorator",decoratorType))

            case _ => throw TouchException("Table type "+typeName+" not supported "+thing.getPositionDescription)

          }

        case _ => ()
      }
    }
  }

  private def findFields(script:parser.Script):List[FieldDeclaration] = {
    (for (dec <- script.declarations) yield {
      dec match {
        case v@parser.VariableDefinition(variable,flags) =>
          val programPoint : ProgramPoint = mkTouchProgramPoint(v)
          val modifiers : List[Modifier] = (flags flatMap {
            case ("is_resource","true") => Some(ResourceModifier)
            case ("readonly","true") => Some(ReadOnlyModifier)
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
          val programPoint : ProgramPoint = mkTouchProgramPoint(act)
          val modifiers : List[Modifier] = Nil
          val isPrivate = (body find {case MetaStatement("private",_) => true; case _ => false}) != None
          val name : MethodIdentifier = TouchMethodIdentifier(ident,isEvent=isEvent,isPrivate=isPrivate)
          val parametricType : List[Type] = Nil
          val arguments : List[List[VariableDeclaration]] =
            List(in map (parameterToVariableDeclaration _), out map (parameterToVariableDeclaration _))
          val returnType : Type = null // WE DO NOT USE RETURN TYPES IN TOUCHDEVELOP. SECOND ELEMENT OF PARAM REPR. OUT PARAMS
          val newBody : ControlFlowGraph = new ControlFlowGraph(programPoint)
          val (_,_,handlers) = addStatementsToCFG(body,newBody)
          val preCond : Statement = null
          val postCond : Statement = null
          handlers ::: List(new MethodDeclaration(programPoint,ownerType,modifiers,name,parametricType,arguments,returnType,newBody,preCond,postCond))
        case _ => Nil
      }
    }).flatten
  }

  private def parameterToVariableDeclaration(parameter:parser.Parameter):VariableDeclaration = {
    val programPoint : ProgramPoint = mkTouchProgramPoint(parameter)
    val variable : Variable = parameterToVariable(parameter)
    val typ : Type = typeNameToType(parameter.typeName)
    val right : Statement = null
    VariableDeclaration(programPoint,variable,typ,right)
  }

  private def parameterToVariable(parameter:parser.Parameter):Variable = {
    val programPoint : ProgramPoint = mkTouchProgramPoint(parameter)
    val id : Identifier = parameterToVariableIdentifier(parameter)
    Variable(programPoint,id)
  }

  private def parameterToVariableIdentifier(parameter:parser.Parameter):VariableIdentifier = {
    val name : String = parameter.ident
    val typ : Type = typeNameToType(parameter.typeName)
    val programPoint : ProgramPoint = mkTouchProgramPoint(parameter)
    VariableIdentifier(name,typ,programPoint)
  }

  private def typeNameToType(typeName:parser.TypeName, isSingleton:Boolean = false):TouchType = {
    if (!isLibraryIdent(typeName.ident)) {
      SystemParameters.compiler.asInstanceOf[TouchCompiler].getType(typeName.ident).getTyp
    } else new TouchType(typeName.ident,isSingleton)
  }

  private def addStatementsToCFG(statements:List[parser.Statement], cfg:ControlFlowGraph):(Int,Int,List[MethodDeclaration]) = {

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
        val (condStart,condEnd,handlersCond) = addStatementsToCFG(List(ExpressionStatement(condition)), cfg)

        val (thenStart,thenEnd,handlersThen) = addStatementsToCFG(thenBody, cfg)
        val (elseStart,elseEnd,handlersElse) = addStatementsToCFG(elseBody, cfg)

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
        val (condStart,condEnd,handlersCond) = addStatementsToCFG(List(ExpressionStatement(condition)), cfg)
        val (bodyStart,bodyEnd,handlersBody) = addStatementsToCFG(body, cfg)

        cfg.addEdge(curNode, condStart, None)
        cfg.addEdge(condEnd, bodyStart, Some(true))
        cfg.addEdge(condEnd, nextNode, Some(false))
        cfg.addEdge(bodyEnd, condStart, None)

        cfg.setNode(curNode,newStatements)
        newStatements = Nil
        newHandlers = newHandlers ::: handlersCond ::: handlersBody
        curNode = nextNode

      case parser.ExpressionStatement(expr) =>

        newStatements = newStatements ::: expressionToStatement(expr) :: Nil

      case b@Box(body) =>

        // TODO: what else?
        val nextNode = cfg.addNode(Nil)
        val (bodyStart,bodyEnd,handlersBody) = addStatementsToCFG(body,cfg)

        cfg.addEdge(curNode, bodyStart, None)
        cfg.addEdge(bodyEnd, nextNode, None)

        cfg.setNode(curNode,newStatements)
        newStatements = Nil
        newHandlers = newHandlers ::: handlersBody
        curNode = nextNode

      case w@WhereStatement(expr,handlerDefs:List[InlineAction]) =>

        // TODO: THIS NEEDS FIXING

        val handlerMap =
          (for (InlineAction(handlerName,_,_,_) <- handlerDefs) yield { ( handlerName -> handlerIdent(handlerName) ) }).toMap

        val handlers = (for (InlineAction(handlerName,inParameters,outParameters,body) <- handlerDefs) yield {
          val handlerMethodName = handlerIdent(handlerName)
          val programPoint : ProgramPoint = mkTouchProgramPoint(w)
          val modifiers : List[Modifier] = Nil
          val name : MethodIdentifier = TouchMethodIdentifier(handlerMethodName,isEvent = true,isPrivate = true)
          val parametricType : List[Type] = Nil
          val arguments : List[List[VariableDeclaration]] = List(inParameters map (parameterToVariableDeclaration _),outParameters map (parameterToVariableDeclaration _))
          val returnType : Type = null
          val newBody : ControlFlowGraph = new ControlFlowGraph(programPoint)
          val (_,_,subHandlers) = addStatementsToCFG(body,newBody)
          val preCond : Statement = null
          val postCond : Statement = null
          subHandlers ::: List(new MethodDeclaration(programPoint,SystemParameters.typ,modifiers,name,parametricType,arguments,returnType,newBody,preCond,postCond))
        }).flatten

        val newExpression = expr match {

          case parser.Access(obj,property,args) =>
            val newArgs = args.map {
              case e@parser.LocalReference(x) =>
                handlerMap.get(x) match {
                  case Some(y) => parser.Literal(parser.TypeName("Handler"),y)
                  case None => e
                }
              case e:parser.Expression => e
            }
            expressionToStatement(parser.Access(obj,property,newArgs))

          case _  => throw TouchException("This where handler statement does not look like I expected it to look.")

        }

        newStatements = newStatements ::: List(newExpression)
        newHandlers = newHandlers ::: handlers

      case _ => throw TouchException("Invalid statement",statement.pos)

    }

    cfg.setNode(curNode,newStatements)
    (firstNode,curNode,newHandlers)

  }

  private def expressionToStatement(expr:parser.Expression):Statement = {

    val pc = mkTouchProgramPoint(expr)
    if (expr == parser.SingletonReference("skip","Nothing")) return EmptyStatement(pc)
    if (expr == parser.SingletonReference("skip","Skip")) return EmptyStatement(pc)

    val typ = typeNameToType(expr.typeName)

    expr match {

      case parser.LocalReference(ident) =>
        Variable(pc,VariableIdentifier(ident,typ,pc))

      case parser.Access(subject,property,args) =>
        val field = FieldAccess(mkTouchProgramPoint(property),List(expressionToStatement(subject)),property.ident,typeNameToType(subject.typeName))
        MethodCall(mkTouchProgramPoint(property),field,Nil,args map (expressionToStatement(_)),typ)

      case parser.Literal(t,value) =>
        if (t.ident == "Number" || t.ident == "Boolean" || t.ident == "String" || t.ident == "Handler") {
          ConstantStatement(pc,value,typ)
        } else throw new TouchException("Literals with type "+t.ident+" do not exist")

      case parser.SingletonReference(singleton,typ) =>
        Variable(pc,VariableIdentifier(singleton,typeNameToType(expr.typeName,true),pc))

    }

  }

  def handlerIdent(ident:String) = "*handler "+ident
  def isHandlerIdent(ident:String) = ident.startsWith("*handler ")
  def globalReferenceIdent(ident:String) = "*data "+ident
  def isGlobalReferenceIdent(ident:String) = ident.startsWith("*data ")
  def tupleIdent(a:ProgramPoint) = "*tuple "+a.getLine()+" "+a.getColumn()
  def isTupleIdent(ident:String) = ident.startsWith("*tuple ")
  def paramIdent(ident:String) = "*param "+ident
  def isParamIdent(ident:String) = ident.startsWith("*param ")
  def libraryIdent(ident:String) = "♻"+ident
  def isLibraryIdent(ident:String) = ident.startsWith("♻") && ident.length() > 1
  def getLibraryName(ident:String) = ident.substring(1)
  def returnIdent(ident:String) = "*returned "+ident
  def isReturnIdent(ident:String) = ident.startsWith("*returned ")

}


abstract class Named {

  def getName() : String

  override def equals(o : Any) : Boolean = o match {
    case x : Named => x.getName().equals(getName()) && x.getClass.equals(this.getClass)
    case _ => false
  }

  override def hashCode() : Int = getName().hashCode()

  override def toString = getName()
}

case class TouchException(msg:String,pos:Position = null) extends Exception {
  override def toString:String = msg + " (Position: " + pos + ")"
}

case class UnsupportedLanguageFeatureException(msg:String) extends Exception {
  override def toString:String = msg
}

case class TouchPackageIdentifier() extends PackageIdentifier

//class TouchClassDefinition(programpoint_ : ProgramPoint, typ_ : Type, modifiers_ : List[Modifier], name_ : ClassIdentifier,
//                       parametricTypes_ : List[Type], extend_ : List[ClassIdentifier], fields_ : List[FieldDeclaration],
//                       methods_ : List[MethodDeclaration], pack_ : PackageIdentifier, inv_ : Expression
//                       ) extends ClassDefinition(programpoint_,typ_,modifiers_,name_,parametricTypes_,extend_,fields_,
//                                  methods_,pack_,inv_)
//{
//
//
//
//}


case class TouchMethodIdentifier(ident:String,isEvent:Boolean,isPrivate:Boolean) extends MethodIdentifier {
  override def toString:String = ident
}

case class TouchClassIdentifier(name:String,typ:Type) extends Named with ClassIdentifier {
  def getThisType() = typ
  override def getName() = name
}

case class TouchProgramPoint(scriptID:String, pos:String) extends ProgramPoint {
  def getScriptID:String = scriptID
  def getLine() = 0
  def getColumn() = 0
  override def toString = "{"+getScriptID+","+pos+"}"
}

case class TouchSingletonProgramPoint(name:String) extends ProgramPoint {
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

class TouchType(name:String, val isSingleton:Boolean = false, val isImmutable:Boolean = false, fields: List[Identifier] = List.empty[Identifier]) extends Named with Type {

  var isBottom = false;
  var isTop = false;

  override def getName() = name
  override def toString():String = name

  def factory() = top()
  def top() = { val res = new TouchType("Top"); res.isTop = true; res }
  def bottom() = { val res = new TouchType("Bottom"); res.isBottom = true; res }

  def lub(l : oorepresentation.Type, r : oorepresentation.Type) : oorepresentation.Type = {
    if(l==null) return r
    if(r==null) return l
    val (left, right)=cast(l, r)
    if(left.isTop || right.isTop) return top()
    if(left.isBottom) return right
    if(right.isBottom) return left
    if (!left.equals(right)) top()
    else left
  }

  def glb(l : oorepresentation.Type, r : oorepresentation.Type) : oorepresentation.Type = {
    if(l==null) return r
    if(r==null) return l
    val (left, right)=cast(l, r)
    if(left.isBottom || right.isBottom) return bottom()
    if(left.isTop) return right
    if(right.isTop) return left
    if (!left.equals(right)) bottom()
    else left
  }

  def cast(l : oorepresentation.Type, r : oorepresentation.Type) = {
    if((! l.isInstanceOf[TouchType]) || (! r.isInstanceOf[TouchType]))
      throw new TouchException("Types are not congruent!");
    (l.asInstanceOf[TouchType], r.asInstanceOf[TouchType])
  }

  def widening(left: Type, right: Type) = lub(left,right)
  def lessEqual(r: Type) = r == this || this.isBottom || r == top()
  def isBottomExcluding(types: Set[Type]) = false

  def isObject() = (!isNumericalType())
  def isNumericalType() = (name == "Number") || (name == "Boolean")
  def isFloatingPointType() = (name == "Number")
  def isStringType() = (name == "String")
  def isStatic() = isSingleton
  def getPossibleFields() = fields.toSet[Identifier]
  def getPossibleTouchFields() = fields.toSet[Identifier] map (_.asInstanceOf[TouchField])
  def getArrayElementsType() = None

}

case class TouchCollection(name:String,keyType:String,valueType:String, fields: List[Identifier] = List.empty[Identifier], immutableCollection:Boolean = false) extends TouchType(name,false,immutableCollection,fields) {

  def getKeyType = SystemParameters.compiler.asInstanceOf[TouchCompiler].getType(keyType).getTyp
  def getValueType = SystemParameters.compiler.asInstanceOf[TouchCompiler].getType(valueType).getTyp

}


/**
 * The resource modifier marks any field of a class that represents a preloaded artwork,
 * mostly images loaded from URLs.
 */
case object ResourceModifier extends Modifier
case object ReadOnlyModifier extends Modifier
