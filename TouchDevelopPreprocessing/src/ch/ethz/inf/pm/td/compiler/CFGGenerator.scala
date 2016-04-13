/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.compiler

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{Expression, VariableIdentifier, _}
import ch.ethz.inf.pm.sample.oorepresentation.{ConstantStatement, EmptyStatement, FieldAccess, MethodCall, Statement, Variable, VariableDeclaration, _}
import ch.ethz.inf.pm.td._
import ch.ethz.inf.pm.td.analysis.TouchAnalysisParameters
import ch.ethz.inf.pm.td.parser.{Box, ExpressionStatement, InlineAction, LibraryDefinition, MetaStatement, TypeName, WhereStatement, _}
import ch.ethz.inf.pm.td.transform.{Rewriter}
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable
import scala.util.parsing.input.{NoPosition, Position}


/**
 *
 * Lucas Brutschy
 * Date: 8/22/12
 * Time: 4:21 PM
 *
 */

object CFGGenerator {
  def handlerIdent(ident: String) = "__handler_" + ident

  def isHandlerIdent(ident: String) = ident.startsWith("__handler_")

  def paramIdent(ident: String) = "__param_" + ident

  def isParamIdent(ident: String) = ident.startsWith("__param_")

  def libraryIdent(ident: String) = "♻" + ident

  def isLibraryIdent(ident: String) = ident.startsWith("♻") && ident.length() > 1

  def getLibraryName(ident: String) = ident.substring(1)

  def recordIdent(ident: String) = "⌹" + ident

  def isRecordIdent(ident: String) = ident.startsWith("⌹") && ident.length() > 1

  def getRecordName(ident: String) = ident.substring(1)

  def returnIdent(ident: String) = "__returned_" + ident

  def isReturnIdent(ident: String) = ident.startsWith("__returned_")

  def isNonDetIdent(ident: String) = ident.startsWith("__nondet")

  def isStmtTempIdent(ident: String) = ident.startsWith("__temp")

  def optionalArgumentIdent = "__optional_argument"

  /**
   * Creates a program point
   *
   * @param pubID public id of the library / script
   * @param libraryStableID "this" or the stable ID of the library import
   * @param element aa
   * @return The program point
   */
  def makeTouchProgramPoint(pubID: String, libraryStableID:String, element: IdPositional) = {
    val pos = element.pos match {
      case NoPosition => None
      case p => Some(p)
    }

    if (TouchAnalysisParameters.get.includeLibraryStableComponent) {
      TouchProgramPointRegistry.make(pubID, pos, libraryStableID :: element.customIdComponents)
    } else {
      TouchProgramPointRegistry.make(pubID, pos, element.customIdComponents)
    }
  }
}


class CFGGenerator(compiler: TouchCompiler) extends LazyLogging {

  import ch.ethz.inf.pm.td.compiler.CFGGenerator._

  private var curPubID: String = ""
  private var curScriptName: String = ""
  private var curLibraryStableId: String = ""

  def process(script: parser.Script, pubID: String, libDef: Option[LibraryDefinition] = None): ClassDefinition = {
    //detectUnsupportedScripts(script)
    curPubID = pubID
    libDef match {
      case Some(l@LibraryDefinition(libName, _, _, _, _, _, _, _)) =>
        curScriptName = libraryIdent(libName)
        curLibraryStableId = l.getIdComponents.head
      case None =>
        curScriptName = libraryIdent(pubID)
        curLibraryStableId = "null"
    }
    val programPoint: ProgramPoint = makeTouchProgramPoint(curPubID, curLibraryStableId, script)
    val typ: Type = typeNameToType(TypeName(curScriptName))
    SystemParameters.typ = typ
    val modifiers: List[Modifier] = Nil
    val name: ClassIdentifier = TouchClassIdentifier(curScriptName, typ)
    val parametricTypes: List[Type] = Nil
    val extend: List[ClassIdentifier] = Nil
    val pack: PackageIdentifier = TouchPackageIdentifier()
    val inv: Expression = null
    val classDef = new ClassDefinition(programPoint, typ, modifiers, name, parametricTypes, extend, null, null, pack, inv)
    val fields: List[FieldDeclaration] = findFields(script, classDef)
    val methods: List[MethodDeclaration] = findMethods(script, typ, classDef)
    classDef.fields = fields
    classDef.methods = methods
    classDef
  }

  private def findFields(script: parser.Script, currentClassDef: ClassDefinition): List[FieldDeclaration] = {
    (for (dec <- script.declarations) yield {
      dec match {
        case v@parser.VariableDefinition(variable, flags) =>
          val programPoint: ProgramPoint = makeTouchProgramPoint(curPubID, curLibraryStableId, v)
          val modifiers: List[Modifier] = (flags flatMap {
            case ("is_resource",  Left(true))  => Some(ResourceModifier)
            case ("is_resource",  Left(false)) => None
            case ("is resource",  Left(true))  => Some(ResourceModifier)
            case ("is resource",  Left(false)) => None
            case ("transient",    Left(true))  => Some(TransientModifier)
            case ("transient",    Left(false)) => None
            case ("readonly",     Left(true))  => Some(ReadOnlyModifier)
            case ("readonly",     Left(false)) => None
            case ("cloudenabled", Left(true))  => Some(CloudEnabledModifier)
            case ("cloudenabled", Left(false)) => None
            case x:Any => logger.debug("Unhandled flag: "+x); None
          }).toList
          val name: Variable = parameterToVariable(variable)
          val typ: Type = typeNameToType(variable.typeName)
          Some(new FieldDeclaration(programPoint, modifiers, name, typ))
        case _ => None
      }
    }).flatten
  }

  private def findMethods(script: parser.Script, ownerType: Type, currentClassDef: ClassDefinition): List[MethodDeclaration] = {
    (for (dec <- script.declarations) yield {
      dec match {
        case act@parser.ActionDefinition(ident, in, out, body, isEvent, isPriv) =>
          val programPoint: ProgramPoint = makeTouchProgramPoint(curPubID, curLibraryStableId, act)
          val scope: ScopeIdentifier = ProgramPointScopeIdentifier(programPoint)
          val modifiers: List[Modifier] = Nil
          val isPrivate = isPriv || (body exists {
            case MetaStatement("private", _) => true;
            case _ => false
          })
          val name: MethodIdentifier = TouchMethodIdentifier(ident, isEvent = isEvent, isPrivate = isPrivate)
          val parametricType: List[Type] = Nil
          val arguments: List[List[VariableDeclaration]] =
            List(in map (parameterToVariableDeclaration(_, scope)), out map (parameterToVariableDeclaration(_, scope)))
          val returnType: Type = null // WE DO NOT USE RETURN TYPES IN TOUCHDEVELOP. SECOND ELEMENT OF PARAM REPR. OUT PARAMS
          val newBody: ControlFlowGraph = new ControlFlowGraph(programPoint)
          val (_, _, handlers) = addStatementsToCFG(body, newBody, scope, currentClassDef)
          val preCond: Statement = null
          val postCond: Statement = null
          handlers ::: List(new MethodDeclaration(programPoint, ownerType, modifiers, name, parametricType, arguments,
            returnType, newBody, preCond, postCond, currentClassDef))
        case act@parser.PageDefinition(ident, in, out, initBody, displayBody, isEvent, isPriv) =>
          val programPoint: ProgramPoint = makeTouchProgramPoint(curPubID, curLibraryStableId, act)
          val scope: ScopeIdentifier = ProgramPointScopeIdentifier(programPoint)
          val modifiers: List[Modifier] = Nil
          val isPrivate = isPriv
          val name: MethodIdentifier = TouchMethodIdentifier(ident, isEvent = isEvent, isPrivate = isPrivate)
          val parametricType: List[Type] = Nil
          val arguments: List[List[VariableDeclaration]] =
            List(in map (parameterToVariableDeclaration(_, scope)), out map (parameterToVariableDeclaration(_, scope)))
          val returnType: Type = null // WE DO NOT USE RETURN TYPES IN TOUCHDEVELOP. SECOND ELEMENT OF PARAM REPR. OUT PARAMS
          val newBody: ControlFlowGraph = new ControlFlowGraph(programPoint)
          val (_, _, handlers) = addStatementsToCFG(initBody ::: displayBody, newBody, scope, currentClassDef)
          val preCond: Statement = null
          val postCond: Statement = null
          handlers ::: List(new MethodDeclaration(programPoint, ownerType, modifiers, name, parametricType, arguments,
            returnType, newBody, preCond, postCond, currentClassDef))
        case _ => Nil
      }
    }).flatten
  }

  private def parameterToVariableDeclaration(parameter: parser.Parameter, scope: ScopeIdentifier = EmptyScopeIdentifier): VariableDeclaration = {
    val programPoint: ProgramPoint = makeTouchProgramPoint(curPubID, curLibraryStableId, parameter)
    val variable: Variable = parameterToVariable(parameter, scope)
    val typ: Type = typeNameToType(parameter.typeName)
    VariableDeclaration(programPoint, variable, typ)
  }

  private def parameterToVariable(parameter: parser.Parameter, scope: ScopeIdentifier = EmptyScopeIdentifier): Variable = {
    val programPoint: ProgramPoint = makeTouchProgramPoint(curPubID, curLibraryStableId, parameter)
    val id: VariableIdentifier = parameterToVariableIdentifier(parameter, scope)
    Variable(programPoint, id)
  }

  private def parameterToVariableIdentifier(parameter: parser.Parameter, scope: ScopeIdentifier = EmptyScopeIdentifier): VariableIdentifier = {
    val name: String = parameter.ident
    val typ: Type = typeNameToType(parameter.typeName)
    val programPoint: ProgramPoint = makeTouchProgramPoint(curPubID, curLibraryStableId, parameter)
    VariableIdentifier(name, scope)(typ, programPoint)
  }

  private def typeNameToType(typeName: parser.TypeName): TouchType = TypeList.getTypeOrFail(typeName)

  private def tty(typ: TypeName, expr: parser.Expression): parser.Expression = {
    expr.typeName = typ
    expr
  }

  private def ty(typ: String, expr: parser.Expression): parser.Expression = {
    expr.typeName = TypeName(typ)
    expr
  }

  private def sty(typ: String, expr: parser.Expression): parser.Expression = {
    expr.typeName = TypeName(typ,isSingleton = true)
    expr
  }

  private def addStatementsToCFG(statements: List[parser.Statement], cfg: ControlFlowGraph, scope: ScopeIdentifier,
                                 currentClassDef: ClassDefinition): (Int, Int, List[MethodDeclaration]) = {

    val firstNode = cfg.addNode(Nil)
    var newStatements: List[Statement] = Nil
    var newHandlers: List[MethodDeclaration] = Nil
    var curNode = firstNode

    for (statement <- statements) statement match {

      case s@Break() =>

        throw new UnsupportedLanguageFeatureException("break not supported")

      case s@Return(expr) =>

        throw new UnsupportedLanguageFeatureException("return not supported")

      case s@Continue() =>

        throw new UnsupportedLanguageFeatureException("continue not supported")

      case s@Show(expr) =>

        throw new UnsupportedLanguageFeatureException("show not supported")

      case parser.MetaStatement(_, _) =>
        Unit

      case parser.Skip() =>
        Unit

      case parser.If(condition, thenBody, elseBody) =>

        val nextNode = cfg.addNode(Nil)
        val (condStart, condEnd, handlersCond) = addStatementsToCFG(List(ExpressionStatement(condition)), cfg, scope, currentClassDef)

        val (thenStart, thenEnd, handlersThen) = addStatementsToCFG(thenBody, cfg, scope, currentClassDef)
        val (elseStart, elseEnd, handlersElse) = addStatementsToCFG(elseBody, cfg, scope, currentClassDef)

        cfg.addEdge(curNode, condStart, None)
        cfg.addEdge(condEnd, thenStart, Some(true))
        cfg.addEdge(condEnd, elseStart, Some(false))
        cfg.addEdge(thenEnd, nextNode, None)
        cfg.addEdge(elseEnd, nextNode, None)

        cfg.setNode(curNode, newStatements)
        newStatements = Nil
        newHandlers = newHandlers ::: handlersCond ::: handlersThen ::: handlersElse
        curNode = nextNode

      case parser.While(condition, body) =>

        val nextNode = cfg.addNode(Nil)
        val (condStart, condEnd, handlersCond) = addStatementsToCFG(List(ExpressionStatement(condition)), cfg, scope, currentClassDef)
        val (bodyStart, bodyEnd, handlersBody) = addStatementsToCFG(body, cfg, scope, currentClassDef)

        cfg.addEdge(curNode, condStart, None)
        cfg.addEdge(condEnd, bodyStart, Some(true))
        cfg.addEdge(condEnd, nextNode, Some(false))
        cfg.addEdge(bodyEnd, condStart, None)

        cfg.setNode(curNode, newStatements)
        newStatements = Nil
        newHandlers = newHandlers ::: handlersCond ::: handlersBody
        curNode = nextNode

      case parser.ExpressionStatement(expr) =>

        val newExpr = Rewriter.apply(expr)(onExpression = {
          case p@Placeholder(typName) =>
            tty(typName,Access(
              tty(TypeName("Constructor",List(typName)),Access(
                sty("records",SingletonReference("records", "records").copyPos(p)),
                Identifier(typName.toString).copyPos(p),
                Nil
              ).copyPos(p)),
              Identifier("create").copyPos(p),
              Nil
            ).copyPos(p))
          case x:parser.Expression => x
        })
        newStatements = newStatements ::: expressionToStatement(newExpr, scope) :: Nil


      case b@Box(body) =>

        // TODO: what else?
        val nextNode = cfg.addNode(Nil)
        val (bodyStart, bodyEnd, handlersBody) = addStatementsToCFG(body, cfg, scope, currentClassDef)

        cfg.addEdge(curNode, bodyStart, None)
        cfg.addEdge(bodyEnd, nextNode, None)

        cfg.setNode(curNode, newStatements)
        newStatements = Nil
        newHandlers = newHandlers ::: handlersBody
        curNode = nextNode

      case w@WhereStatement(expr, handlerDefs: List[InlineAction], optionalParameters: List[OptionalParameter]) =>

        val wPP = makeTouchProgramPoint(curPubID, curLibraryStableId, w)

        val handlerSet =
          for (InlineAction(handlerName, inParameters, _, _, typ) <- handlerDefs) yield {
            (handlerName, handlerIdent(handlerName + wPP), typ)
          }

        val handlers = (for (InlineAction(handlerName, inParameters, outParameters, body, typ) <- handlerDefs) yield {
          val handlerMethodName = handlerIdent(handlerName + wPP)
          val programPoint: ProgramPoint = wPP
          val modifiers: List[Modifier] = List(ClosureModifier)
          val name: MethodIdentifier = TouchMethodIdentifier(handlerMethodName, isEvent = true, isPrivate = true)
          val parametricType: List[Type] = Nil
          val arguments: List[List[VariableDeclaration]] =
            List(inParameters map (parameterToVariableDeclaration(_, scope)), outParameters map (parameterToVariableDeclaration(_, scope)))
          val returnType: Type = null
          val newBody: ControlFlowGraph = new ControlFlowGraph(programPoint)
          val (_, _, subHandlers) = addStatementsToCFG(body, newBody, scope, currentClassDef)
          val preCond: Statement = null
          val postCond: Statement = null
          subHandlers ::: List(new MethodDeclaration(programPoint, currentClassDef.typ, modifiers, name, parametricType,
            arguments, returnType, newBody, preCond, postCond, currentClassDef))
        }).flatten

        // Create a statement that creates the handler object and assigns the handler variable
        val handlerCreationStatements = handlerSet map {
          case (variableName: String, actionName: String, handlerType: TypeName) =>
            expressionToStatement(
              ty("Nothing", parser.Access(
                ty(handlerType.toString, parser.LocalReference(variableName).copyPos(w)),
                Identifier(":=").copyPos(w),
                List(
                  ty(handlerType.toString, parser.Access(
                    sty("Helpers", parser.SingletonReference("helpers", "Helpers").copyPos(w)),
                    Identifier("create " + handlerType.ident.toLowerCase + " " + actionName).copyPos(w),
                    Nil
                  ).copyPos(w))
                )
              ).copyPos(w)),
              scope
            )
        }

        val (optionalArgumentCreationStatements,newExpr) =
          if (optionalParameters.nonEmpty) {

            var optParamTypName:Option[(TypeName,Placeholder)] = None
            val neExpr = Rewriter.apply(expr)(onExpression = {
              case p@Placeholder(typName) =>
                optParamTypName = Some((typName,p))
                tty(typName,LocalReference(optionalArgumentIdent).copyPos(p))
              case x:parser.Expression => x
            })

            optParamTypName match {
              case Some((typName,p)) =>

                val initialization =
                  expressionToStatement(
                    ty("Nothing",Access(
                      tty(typName,LocalReference(optionalArgumentIdent)),
                      Identifier(":="),
                      List(
                        tty(typName,Access(
                          tty(TypeName("Constructor",List(typName)),Access(
                            sty("records",SingletonReference("records", "records").copyPos(p)),
                            Identifier(typName.toString).copyPos(p),
                            Nil
                          ).copyPos(p)),
                          Identifier("create").copyPos(p),
                          Nil
                        ).copyPos(p))
                      )
                    ).copyPos(p)),
                    scope
                  ) ::
                    optionalParameters.map { x: OptionalParameter =>
                      expressionToStatement(
                        ty("Nothing",Access(
                          tty(x.expr.typeName,Access(
                            tty(typName,LocalReference(optionalArgumentIdent).copyPos(x.expr)),
                            Identifier(x.name).copyPos(x.expr),
                            Nil
                          ).copyPos(x.expr)),
                          Identifier(":="),
                          List(
                            x.expr
                          )
                        ).copyPos(x.expr)),
                        scope
                      )
                    }

                (initialization,neExpr)

              case None => throw new TouchException("Optional Parameters but no placeholder")
            }

          } else {

            val neExpr = Rewriter.apply(expr)(onExpression = {
              case p@Placeholder(typName) =>
                tty(typName,Access(
                  tty(TypeName("Constructor",List(typName)),Access(
                    sty("records",SingletonReference("records", "records").copyPos(p)),
                    Identifier(typName.toString).copyPos(p),
                    Nil
                  ).copyPos(p)),
                  Identifier("create").copyPos(p),
                  Nil
                ))
              case x:parser.Expression => x
            })

            (Nil,neExpr)

          }

        newStatements = newStatements ::: handlerCreationStatements ::: optionalArgumentCreationStatements ::: List(expressionToStatement(newExpr, scope))
        newHandlers = newHandlers ::: handlers

      case _ =>
        throw TouchException("Invalid statement", statement.pos)

    }

    cfg.setNode(curNode, newStatements)
    (firstNode, curNode, newHandlers)

  }

  private def expressionToStatement(expr: parser.Expression, scope: ScopeIdentifier): Statement = {

    val pc = makeTouchProgramPoint(curPubID, curLibraryStableId, expr)
    if (expr == parser.SingletonReference("skip", "Nothing")) return EmptyStatement(pc)
    if (expr == parser.SingletonReference("skip", "Skip")) return EmptyStatement(pc)

    val typ = typeNameToType(expr.typeName)

    expr match {

      case parser.Placeholder(typeName) =>
        Variable(pc, VariableIdentifier(optionalArgumentIdent, scope)(typ,pc))

      case parser.LocalReference(ident) =>
        Variable(pc, VariableIdentifier(ident, scope)(typ, pc))

      case parser.Access(subject, property, args) =>
        val field = FieldAccess(makeTouchProgramPoint(curPubID, curLibraryStableId, property), expressionToStatement(subject, scope), property.ident, typeNameToType(subject.typeName))
        MethodCall(makeTouchProgramPoint(curPubID, curLibraryStableId, property), field, Nil, args map (expressionToStatement(_, scope)), typ)

      case parser.Literal(t, value) =>
        if (t.ident == "Number" || t.ident == "Boolean" || t.ident == "String" || t.ident == "Handler") {
          ConstantStatement(pc, value, typ)
        } else throw new TouchException("Literals with type " + t.ident + " do not exist")

      case parser.SingletonReference(singleton, _) =>
        Variable(pc, VariableIdentifier(singleton)(typeNameToType(expr.typeName), pc))
    }

  }
}


trait Named {
  def name: String

  override def equals(o: Any): Boolean = o match {
    case x: Named => x.name == name
    case _ => false
  }

  override def hashCode: Int = name.hashCode()

  override def toString = name
}

case class TouchException(msg: String, pos: Position = null) extends Exception {
  override def toString: String = msg + " (Position: " + pos + ")"
}

case class UnsupportedLanguageFeatureException(msg: String) extends Exception {
  override def toString: String = msg
}

case class TouchPackageIdentifier() extends PackageIdentifier

case class TouchMethodIdentifier(ident: String, isEvent: Boolean, isPrivate: Boolean) extends MethodIdentifier {
  override def toString: String = ident
}

case class TouchClassIdentifier(name: String, typ: Type) extends Named with ClassIdentifier {
  def getThisType() = typ
}

/** Used to speed up the analysis */
object TouchProgramPointRegistry {

  /** inefficient, knowingly so */
  def get(scriptID: String, positional: IdPositional): Option[TouchProgramPoint] = {
    revReg.get((scriptID,Some(positional.pos),positional.customIdComponents))
  }

  def matches(point: SpaceSavingProgramPoint, scriptID: String, positional: IdPositional): Boolean = {
    val pp = reg(point.id)
    pp.scriptID == scriptID &&
      ((positional.pos == NoPosition && pp.lineColumnPosition.isEmpty) || pp.lineColumnPosition.contains(positional.pos)) &&
      (positional.customIdComponents == pp.customPositionElements)
  }

  val reg = mutable.ArrayBuffer.empty[TouchProgramPoint]
  val revReg = mutable.HashMap.empty[(String,Option[Position],List[String]),TouchProgramPoint]

  def make(scriptID: String,
           lineColumnPosition: Option[Position],
           customPositionElements: List[String]): SpaceSavingProgramPoint = {
    val pp = new TouchProgramPoint(scriptID, lineColumnPosition, customPositionElements)
    reg += pp
    revReg += ((scriptID,lineColumnPosition,customPositionElements) -> pp)
    SpaceSavingProgramPoint(reg.length - 1)
  }

  def reset() {
    reg.clear()
    revReg.clear()
  }

}

case class SpaceSavingProgramPoint(id: Int) extends ProgramPoint {

  /**
   * Gets a description of the program point, for example
   * "in file somefile.scala at line 1, column 2"
   */
  override def description: String = TouchProgramPointRegistry.reg.apply(id).description

  def fullPosString: String = TouchProgramPointRegistry.reg.apply(id).fullPosString

  override def toString: String = TouchProgramPointRegistry.reg(id).fullPosString

}

case class TouchProgramPoint(
                              scriptID: String,
                              lineColumnPosition: Option[Position],
                              customPositionElements: List[String])
  extends ProgramPoint {

  def fullPosString: String = {
    val parserPos = lineColumnPosition
      .map(_.toString())
      .getOrElse("")
    val customPos = customPositionElements.mkString("_")
    parserPos + customPos
  }

  override def toString = {
    val fullPos = fullPosString
    s"PP($scriptID:$fullPos)"
  }

  override def description = {
    val fullPos = fullPosString
    s"in script $scriptID at node $fullPos"
  }
}

case class TouchSingletonProgramPoint(name: String) extends ProgramPoint {
  override def toString = "Init(" + name + ")"

  override def description = "at initialization of singleton " + name
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
  def apply(pp: ProgramPoint, name: String): (DeepeningProgramPoint, Boolean) = {
    pp match {
      case DeepeningProgramPoint(realPP, path) =>
        if (path.contains(name)) (DeepeningProgramPoint(realPP, path.takeWhile(!_.equals(name)) ::: (name :: Nil)), true)
        else (DeepeningProgramPoint(realPP, path ::: (name :: Nil)), false)
      case _ => (DeepeningProgramPoint(pp, name :: Nil), false)
    }
  }
}

case class DeepeningProgramPoint(pp: ProgramPoint, path: List[String]) extends ProgramPoint {
  override def toString = pp + "(" + path.mkString(",") + ")"

  override def description = pp + " at initialization path " + path.mkString(",")
}

/**
 * The resource modifier marks any field of a class that represents a preloaded artwork,
 * mostly images loaded from URLs.
 */
case object ResourceModifier extends Modifier

case object ReadOnlyModifier extends Modifier

/**
 * Does not persist between executions
 */
case object TransientModifier extends Modifier

/**
 * Is synched between devices
 */
case object CloudEnabledModifier extends Modifier
case object PartialCloudEnabledModifier extends Modifier
case object ExportedModifier extends Modifier
