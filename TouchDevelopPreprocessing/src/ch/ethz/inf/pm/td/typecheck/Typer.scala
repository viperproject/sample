package ch.ethz.inf.pm.td.typecheck

import ch.ethz.inf.pm.td.analysis.ApiField
import ch.ethz.inf.pm.td.parser
import ch.ethz.inf.pm.td.parser._
import ch.ethz.inf.pm.td.compiler.{TypeList, CFGGenerator, TouchException}
import ch.ethz.inf.pm.td.semantics._

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
    val symbolTable = new SymbolTable(script)
    processScript(script, symbolTable)
    symbolTable
  }

  def processScript(script: Script, symbolTable: SymbolTable) {
    for (declaration <- script.declarations) addGlobals(script, symbolTable, declaration)
    for (declaration <- script.declarations) typeLocals(script, symbolTable, declaration)
  }

  def addGlobals(scope: Script, st: SymbolTable, thing: Declaration) {

    def toTouchField(fields: List[Parameter]): List[ApiField] = {
      for (field <- fields) yield {
        ApiField(field.ident, TypeList.getTypeOrFail(field.typeName))
      }
    }

    thing match {
      case a@ActionType(name, in, out, body, isPrivate) =>
        TypeList.addTouchType(GAction(TypeName(name),in map {x:Parameter => TypeList.getTypeOrFail(x.typeName)},out map {x:Parameter => TypeList.getTypeOrFail(x.typeName)}))
      case a@ActionDefinition(name, inParameters, outParameters, body, isEvent, isPrivate) =>
        st.addAction(name, inParameters, outParameters)
      case PageDefinition(name, inParameters, outParameters, initBody, displayBody, isPrivate) =>
        st.addAction(name, Nil, Nil) // We ignore parameters of pages - they ware initialized in the display code
      case VariableDefinition(Parameter(name, kind), flags) =>
        st.addGlobalData(name, kind)
      case TableDefinition(ident, typeName, keys, fields) =>
        typeName match {
          case "object" =>

            val objectType = GObject(TypeName(ident),toTouchField(fields))
            val objectConstructor = GObjectConstructor(objectType)
            val objectCollection = GObjectCollection(objectType)

            TypeList.addTouchType(objectType)
            TypeList.addTouchType(objectCollection)
            TypeList.addTouchType(objectConstructor)
            TypeList.addRecord(ident, objectConstructor)

          case "table" =>

            val rowTyp = GRow(TypeName(ident), toTouchField(fields))
            val tableType = GTable(rowTyp)

            TypeList.addTouchType(rowTyp)
            TypeList.addTouchType(tableType)
            TypeList.addRecord(ident + " table", tableType)

          case "index" =>

            val keyMembers = toTouchField(keys)
            val fieldMembers = toTouchField(fields)
            val indexMember = GIndexMember(TypeName(ident), keyMembers, fieldMembers)
            val indexType =
              if (keyMembers.size > 0) {
                GIndex(keyMembers.map{_.typ},indexMember)
              } else {
                GSingletonIndex(indexMember)
              }

            TypeList.addTouchType(indexMember)
            TypeList.addTouchType(indexType)
            TypeList.addRecord(ident + " index", indexType)

          case "decorator" =>

            if (keys.size != 1) throw TouchException("Decorators must have exactly one entry " + thing.getPositionDescription)

            val keyMembers = toTouchField(keys)
            val fieldMembers = toTouchField(fields)
            val indexMember = GIndexMember(TypeName(ident),keyMembers,fieldMembers)

            val decoratedType = keyMembers.head.typ
            val decoratorType = GDecorator(TypeName(decoratedType.name.toString + " Decorator"), decoratedType, indexMember)

            TypeList.addTouchType(indexMember)
            TypeList.addTouchType(decoratorType)
            TypeList.addRecord(decoratedType + " decorator", decoratorType)

          case _ => throw TouchException("Table type " + typeName + " not supported " + thing.getPositionDescription)

        }
      case LibraryDefinition(libName, pub, usages, _, _, resolves) =>
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
          st(scope) = st(scope) + (h.handlerName -> h.typ)
          for (smt <- h.body) processStatement(s, st, smt)
        }
        processMultiValExpression(scope, st, expr)
      case ExpressionStatement(expr) =>
        processMultiValExpression(scope, st, expr)
      case _ => Unit
    }
  }

  def processExpression(scope: Scope, st: SymbolTable, expr: Expression): TypeName = {

    def is(typ: TypeName): TypeName = {
      expr.typeName = typ
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
