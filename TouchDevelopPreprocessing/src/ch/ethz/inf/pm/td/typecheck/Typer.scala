package ch.ethz.inf.pm.td.typecheck

import ch.ethz.inf.pm.td.parser._
import ch.ethz.inf.pm.td.compiler.{CFGGenerator, TouchException}

/**
 *
 * Checks types in the scripts and attaches type information to every expression
 * Only the main script are checked and annotated, none of the libraries
 *
 * Lucas Brutschy
 * Date: 8/7/12
 * Time: 3:42 PM
 *
 */

object Typer {

  def processScript(script: Script): SymbolTable = {
    val symbolTable = new SymbolTable(script) with StdLib with DebugLib
    processScript(script, symbolTable)
    symbolTable
  }

  def processScript(script: Script, symbolTable: SymbolTable) {
    for (declaration <- script.declarations) addGlobals(script, symbolTable, declaration)
    for (declaration <- script.declarations) typeLocals(script, symbolTable, declaration)
  }


  def addGlobals(scope: Script, st: SymbolTable, thing: Declaration) {
    thing match {
      case a@ActionDefinition(name, inParameters, outParameters, body, isEvent, isPrivate) =>
        st.addAction(name, inParameters, outParameters)
      case PageDefinition(name, inParameters, outParameters, initBody, displayBody, isPrivate) =>
        st.addAction(name, inParameters, outParameters)
      case VariableDefinition(Parameter(name, kind), flags) =>
        st.addGlobalData(name, kind)
      case TableDefinition(ident, typeName, keys, fields) =>
        val fieldMembers = for (field <- fields) yield {
          Member(field.ident, field.typeName)
          //          val inp = field.typeName.toString
          //          val (fieldType, noFieldType) =
          //            if(inp.matches(""" field$""")) {
          //              val (part1,check) = inp.splitAt(inp.size-7)
          //              if (check != " field") throw TouchException("Expected field here")
          //              (part1+" field",part1)
          //            } else {
          //              (inp+" field",inp)
          //            }
          //          if (noFieldType == "Number") st.addUserType(fieldType, GenericTypes.gNumberField(fieldType))
          //          else st.addUserType(fieldType, GenericTypes.gField(fieldType, noFieldType))
          //          Member(field.ident, fieldType)
        }

        typeName match {
          case "object" =>
            st.addUserType(ident, GenericTypes.gObject(ident, fieldMembers))
            st.addUserType(ident + " Collection", GenericTypes.gMutableCollection(ident + " Collection", ident))
            st.addUserType(ident + " Constructor", List(Member("create", ident),
              Member("create collection", ident + " Collection"),
              Member("invalid", ident)))
            st.addUserSingleton("records", List(Member(ident, ident + " Constructor")))
          case "table" =>
            st.addUserType(ident, GenericTypes.gRow(ident, fieldMembers))
            st.addUserType(ident + " Table", GenericTypes.gTable(ident + " Table", ident))
            st.addUserSingleton("records", List(Member(ident + " table", ident + " Table")))
          case "index" =>
            val keyMembers = keys map {
              case Parameter(x, typ) => Member(x, typ)
            }
            val keyTypes = keys map {
              case Parameter(_, typ) => typ.toString
            }
            val fieldAndKeyMembers = fieldMembers ::: keyMembers
            st.addUserType(ident, GenericTypes.gIndexMember(ident, fieldAndKeyMembers))
            st.addUserType(ident + " Index", GenericTypes.gIndex(ident + " Index", keyTypes, ident))
            st.addUserSingleton("records", List(Member(ident + " index", ident + " Index")))
          case "decorator" =>
            if (keys.size != 1) throw TouchException("Decorators must have exactly one entry", thing.pos)
            val decoratedType = keys.head.typeName.toString
            val keyMembers = keys map {
              case Parameter(x, typ) => Member(x, typ)
            }
            val fieldAndKeyMembers = fieldMembers ::: keyMembers
            st.addUserType(ident, GenericTypes.gIndexMember(ident, fieldAndKeyMembers))
            st.addUserType(decoratedType + " Decorator",
              GenericTypes.gIndex(decoratedType + " Decorator", List(decoratedType), ident))
            st.addUserSingleton("records", List(Member(decoratedType + " decorator", decoratedType + " Decorator")))
          case _ => throw TouchException("Table type " + typeName + " not supported", thing.pos)

        }
      case LibraryDefinition(libName, pub, usages, resolves) =>
        for (usage <- usages) {
          usage match {
            case ActionUsage(name, in, out) => st.addLibAction(libName, name, in, out)
            case TypeUsage(ident) => throw TouchException("Type usages are unsupported.")
          }
        }
      case _ => Unit
    }
  }

  def typeLocals(scope: Script, st: SymbolTable, thing: Declaration) {
    thing match {
      case a@ActionDefinition(name, inParameters, outParameters, body, isEvent, isPrivate) =>
        st(a) = ScopeSymbolTable(a, null, Map.empty) ++ inParameters ++ outParameters
        for (smt <- body) processStatement(a, st, smt)
      case p@PageDefinition(name, inParameters, outParameters, initBody, displayBody, isPrivate) =>
        st(p) = ScopeSymbolTable(p, null, Map.empty) ++ inParameters ++ outParameters
        for (smt <- initBody) processStatement(p, st, smt)
        for (smt <- displayBody) processStatement(p, st, smt)
      case _ => Unit
    }
  }

  def processStatement(scope: Scope, st: SymbolTable, statement: Statement) {
    statement match {
      case s@While(cond, body) =>
        st(s) = ScopeSymbolTable(s, scope, Map.empty)
        processExpression(s, st, cond)
        for (smt <- body) processStatement(s, st, smt)
      case s@If(cond, thenBody, elseBody) =>
        st(s) = ScopeSymbolTable(s, scope, Map.empty)
        processExpression(s, st, cond)
        for (smt <- thenBody) processStatement(s, st, smt)
        for (smt <- elseBody) processStatement(s, st, smt)
      case s@Box(body) =>
        st(s) = ScopeSymbolTable(s, scope, Map.empty)
        for (smt <- body) processStatement(s, st, smt)
      case s@WhereStatement(expr, handlers) =>
        for (h <- handlers) {
          st(s) = ScopeSymbolTable(s, scope, Map.empty) ++ h.inParameters ++ h.outParameters
          st(scope) = st(scope) + (h.handlerName -> inParametersToActionType(h.inParameters))
          for (smt <- h.body) processStatement(s, st, smt)
        }
        processMultiValExpression(scope, st, expr)
      case ExpressionStatement(expr) =>
        processMultiValExpression(scope, st, expr)
      case _ => Unit
    }
  }

  def inParametersToActionType(params: List[Parameter]): TypeName = {
    TypeName(params match {
      case Nil => "Action"
      case List(Parameter(_, TypeName("Boolean",_))) => "Boolean Action"
      case List(Parameter(_, TypeName("Number",_))) => "Number Action"
      case List(Parameter(_, TypeName("Number",_)), Parameter(_, TypeName("Number",_))) => "Position Action"
      case List(Parameter(_, TypeName("String",_))) => "Text Action"
      case List(Parameter(_, TypeName("Sprite",_))) => "Sprite Action"
      case List(Parameter(_, TypeName("Sprite Set",_))) => "Sprite Set Action"
      case List(Parameter(_, TypeName("Number",_)), Parameter(_, TypeName("Number",_)), Parameter(_, TypeName("Number",_)), Parameter(_, TypeName("Number",_))) => "Vector Action"
      case List(Parameter(_, TypeName("Web Response",_))) => "Web Response Action"
      case List(Parameter(_, TypeName("Message Collection",_))) => "Message Collection Action"
      case List(Parameter(_, TypeName(s,_))) => s + " action"
      case _ => println("Unknown action type, falling back to Action: " + params); "Action"
    })
  }

  def processExpression(scope: Scope, st: SymbolTable, expr: Expression): TypeName = {

    def is(typ: TypeName): TypeName = {
      expr.typeName = typ;
      typ
    }

    def handleAssignments(variables: Expression, types: List[TypeName]): Int = {

      if (types.length == 0) throw TouchException("Not enough values on the right side of the assignment", expr.pos)
      val typ = types.head

      variables match {
        case Access(left, Identifier(","), List(right)) =>
          variables.typeName = TypeName("Unknown")
          val noVariablesLeft = handleAssignments(left, types)
          handleAssignments(right, types.splitAt(types.length - noVariablesLeft)._2)
        case l@LocalReference(ident) =>
          variables.typeName = typ
          st.tryResolveLocal(scope, ident) match {
            case Some(x) =>
              if (x != typ)
                throw TouchException("Assignment to wrong type. Expected: " + x + ", Found: " + types.head, variables.pos)
            case None =>
              st(scope) = st(scope) + (ident -> typ)
          }
          types.length - 1
        case Access(subject@SingletonReference("data", _), Identifier(ident), Nil) =>
          processExpression(scope, st, subject)
          variables.typeName = typ
          val expType = st.resolveData(ident, variables.pos)
          if (expType != typ)
            throw TouchException("Assignment to wrong type. Expected: " + expType + ", Found: " + types.head, variables.pos)
          types.length - 1
        // New kind of field assignment for record types?!
        case Access(subject, _, args) =>
          // TODO: Actually check the type
          for (arg <- args) yield processExpression(scope, st, arg)
          processExpression(scope, st, subject)
          variables.typeName = typ
          types.length - 1
      }

    }

    val typ = expr match {
      case Access(subject, property, args) =>
        property match {
          case Identifier(":=") =>
            if (args.length != 1) throw TouchException("Assignment needs an argument")
            val right = processMultiValExpression(scope, st, args(0))
            val additionalRightSideValues = handleAssignments(subject, right)
            if (additionalRightSideValues != 0)
              throw TouchException("Not enough variables on the left side of the assignment", expr.pos)
            is(TypeName("Unknown"))
          case _ =>
            val types = for (arg <- args) yield processExpression(scope, st, arg)
            val subtype = processExpression(scope, st, subject)
            val retTypes = st.resolveAccess(subtype, property.ident, types, subject.pos)
            if (retTypes.length > 1) throw TouchException("Multiple return values " + retTypes + " in non-assignment expression", expr.pos)
            else if (retTypes.length < 1) is(TypeName("Nothing"))
            else is(retTypes.head)
        }
      case l@LocalReference(ident) =>
        // Interestingly, contract is a local variable, not a singleton.
        if (ident == "contract") is(TypeName("Contract"))
        else is(st.resolveLocal(scope, ident, l.pos))
      case SingletonReference(singleton, typX) =>
        if (CFGGenerator.isLibraryIdent(typX)) {
          is(TypeName("♻"))
        } else {
          is(TypeName(typX))
        }
      case Literal(typX, _) =>
        is(typX)
    }

    typ
  }

  def processMultiValExpression(scope: Scope, st: SymbolTable, expr: Expression): List[TypeName] = {

    def is(typ: TypeName): TypeName = {
      expr.typeName = typ
      typ
    }

    val types = expr match {
      case Access(_, Identifier(","), List(left, right)) =>
        processMultiValExpression(scope, st, left) ::: processMultiValExpression(scope, st, right)
      case Access(subject@SingletonReference("code", _), action, args) =>
        processExpression(scope, st, subject)
        val types = for (arg <- args) yield processExpression(scope, st, arg)
        st.resolveCode(action.ident, types, expr.pos)
      case Access(a@Access(SingletonReference("♻", _), lib, Nil), action, args) =>
        processExpression(scope, st, a)
        val types = for (arg <- args) yield processExpression(scope, st, arg)
        st.resolveLib(lib.ident, action.ident, types, expr.pos)
      case _ => List(processExpression(scope, st, expr))
    }

    if (types.length == 1) {
      is(types.head)
    } else if (types.length == 0) {
      is(TypeName("Nothing"))
    } else {
      is(TypeName("Unknown"))
    }

    types
  }


}
