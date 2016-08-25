/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.typecheck

import ch.ethz.inf.pm.sample.oorepresentation.Modifier
import ch.ethz.inf.pm.td.parser._
import ch.ethz.inf.pm.td.compiler._
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
    for (declaration <- script.declarations) addTypes(script, symbolTable, declaration)
    for (declaration <- script.declarations) addGlobals(script, symbolTable, declaration)
    for (declaration <- script.declarations) typeLocals(script, symbolTable, declaration)
  }

  def addTypes(scope: Script, st: SymbolTable, thing: Declaration): Unit = {

    thing match {
      case a@ActionType(name, in, out, isPrivate) =>
        TypeList.addTouchType(GAction(TypeName(name,isUserDefined = true),in map (_.typeName),out map (_.typeName)))
      case TableDefinition(ident, typeName, keys, fields, isCloudEnabled, isCloudPartiallyEnabled, isPersistent, isExported) =>

        val modifiers = Set[Option[Modifier]](
          if (isCloudEnabled) Some(CloudEnabledModifier) else None,
          if (isCloudPartiallyEnabled) Some(PartialCloudEnabledModifier) else None,
          if (isExported) Some(ExportedModifier) else None,
          if (isPersistent) None else Some(TransientModifier)
        ).flatten

        typeName match {
          case "object" =>

            val objectType = GObject(TypeName(ident,isUserDefined = true),fields,modifiers)
            val objectConstructor = GObjectConstructor(objectType,modifiers)
            val objectCollection = GObjectCollection(objectType,modifiers)

            TypeList.addTouchType(objectType)
            TypeList.addTouchType(objectCollection)
            TypeList.addTouchType(objectConstructor)
            SRecords.addRecord(Record(ident,objectConstructor,modifiers))

          case "table" =>

            val rowTyp = GRow(TypeName(ident,isUserDefined = true), keys, fields,modifiers)
            val tableType = GTable(rowTyp,modifiers)

            TypeList.addTouchType(rowTyp)
            TypeList.addTouchType(tableType)
            SRecords.addRecord(Record(ident + " table",tableType,modifiers))

          case "index" =>

            val indexMember = GIndexMember(TypeName(ident,isUserDefined = true), keys, fields,modifiers)
            val indexType =
              if (keys.nonEmpty) {
                GIndex(keys.map{_.typeName},indexMember,modifiers)
              } else {
                GSingletonIndex(indexMember,modifiers)
              }

            TypeList.addTouchType(indexMember)
            TypeList.addTouchType(indexType)
            SRecords.addRecord(Record(ident + " index",indexType,modifiers))

          case "decorator" =>

            if (keys.size != 1) throw TouchException("Decorators must have exactly one entry " + thing.getPositionDescription)

            val indexMember = GIndexMember(TypeName(ident,isUserDefined = true),keys,fields,modifiers)

            val decoratedType = keys.head.typeName
            val decoratorType = GDecorator(TypeName(decoratedType.toString + " Decorator",isUserDefined = true), decoratedType, indexMember, modifiers)

            TypeList.addTouchType(indexMember)
            TypeList.addTouchType(decoratorType)
            SRecords.addRecord(Record(decoratedType + " decorator",decoratorType,modifiers))

          case _ => throw TouchException("Table type " + typeName + " not supported " + thing.getPositionDescription)

        }
      case LibraryDefinition(libName, pub, _, _, _, _, usages, _) =>
        for (usage <- usages) {
          usage match {
            case ActionUsage(name, in, out) => st.addLibAction(libName, name, in, out)
          }
        }
      case _ => ()
    }

  }

  def addGlobals(scope: Script, st: SymbolTable, thing: Declaration) {

    thing match {
      case a@ActionDefinition(name, inParameters, outParameters, body, isEvent, isPrivate) =>
        st.addAction(name, inParameters, outParameters)
      case a@PageDefinition(name, inParameters, outParameters, initBody, displayBody, isEvent, isPrivate) =>
        st.addAction(name, inParameters, outParameters)
      case VariableDefinition(Parameter(name, kind), flags) =>
        st.addGlobalData(name, kind)
      case LibraryDefinition(libName, pub, _, _, _, _, usages, resolves) =>
        for (usage <- usages) {
          usage match {
            case ActionUsage(name, in, out) => st.addLibAction(libName, name, in, out)
          }
        }
      case _ => ()
    }
  }

  def typeLocals(scope: Script, st: SymbolTable, thing: Declaration) {
    thing match {
      case a@ActionDefinition(name, inParameters, outParameters, body, isEvent, isPrivate) =>
        st(a) = ScopeSymbolTable(a, null, Map.empty) ++ inParameters ++ outParameters
        for (smt <- body) processStatement(a, st, smt)
      case a@PageDefinition(name, inParameters, outParameters, initBody, displayBody, isEvent, isPrivate) =>
        st(a) = ScopeSymbolTable(a, null, Map.empty) ++ inParameters ++ outParameters
        for (smt <- initBody ++ displayBody) processStatement(a, st, smt)
      case _ => ()
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
      case s@WhereStatement(expr, handlers, optParams) =>
        for (h <- handlers) {
          st(s) = ScopeSymbolTable(s, scope, Map.empty) ++ h.inParameters ++ h.outParameters
          st(scope) = st(scope) + (h.handlerName -> h.typ)
          for (smt <- h.body) processStatement(s, st, smt)
        }
        for (o <- optParams) processMultiValExpression(scope,st,o.expr)
        processMultiValExpression(scope, st, expr)
      case ExpressionStatement(expr) =>
        processMultiValExpression(scope, st, expr)
      case _ => ()
    }
  }

  def processExpression(scope: Scope, st: SymbolTable, expr: Expression): TypeName = {

    def is(typ: TypeName): TypeName = {
      expr.typeName = typ
      typ
    }

    def handleAssignments(variables: Expression, types: List[TypeName]): Int = {

      if (types.isEmpty) throw TouchException("Not enough values on the right side of the assignment", expr.pos)
      val typ = types.head

      variables match {
        case Access(left, Identifier(","), List(right)) =>
          variables.typeName = TypeName("Unknown")
          val noVariablesLeft = handleAssignments(left, types)
          handleAssignments(right, types.splitAt(types.length - noVariablesLeft)._2)
        case p@Placeholder(typName) =>
          variables.typeName = typName
          types.length - 1
        case l@LocalReference(ident) =>
          variables.typeName = typ
          st.tryResolveLocal(scope, ident) match {
            case Some(x) =>
              if (x != typ)
                throw TouchException("Assignment to wrong type. Expected: " + x + ", Found: " + types.head + ", in Expr: " + expr, variables.pos)
            case None =>
              st(scope) = st(scope) + (ident -> typ)
          }
          types.length - 1
        case Access(subject@SingletonReference("data", _), Identifier(ident), Nil) =>
          processExpression(scope, st, subject)
          variables.typeName = typ
          val expType = st.resolveData(ident, variables.pos)
          if (expType != typ)
            throw TouchException("Assignment to wrong type. Expected: " + expType + ", Found: " + types.head + ", in Expr: " + expr, variables.pos)
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
            val right = processMultiValExpression(scope, st, args.head)
            val additionalRightSideValues = handleAssignments(subject, right)
            if (additionalRightSideValues != 0)
              throw TouchException("Not enough variables on the left side of the assignment", expr.pos)
            is(TypeName("Unknown"))
          case _ =>
            val types = for (arg <- args) yield processExpression(scope, st, arg)
            val subtype = processExpression(scope, st, subject)
            val retTypes = st.resolveAccess(subtype, property.ident, types, subject.pos)
            if (retTypes.length > 1)
              throw TouchException("Multiple return values " + retTypes + " in non-assignment expression", expr.pos)
            else if (retTypes.length < 1) is(TypeName("Nothing"))
            else is(retTypes.head)
        }
      case p@Placeholder(typName) =>
        is(typName)
      case l@LocalReference(ident) =>
        // Interestingly, contract and cloud storage is a local variable, not a singleton.
        if (ident == "contract") is(TypeName("Contract",isSingleton = true))
        else if (ident == "cloud storage") is(TypeName("Cloud Storage",isSingleton = true))
        else is(st.resolveLocal(scope, ident, l.pos))
      case SingletonReference(singleton, typX) =>
        if (CFGGenerator.isLibraryIdent(typX)) {
          is(TypeName("♻",isSingleton = true))
        } else if (singleton == "$skip") {
          is(TypeName("Unknown"))
        } else {
          is(TypeName(typX,isSingleton = true))
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
    } else if (types.isEmpty) {
      is(TypeName("Nothing"))
    } else {
      is(TypeName("Unknown"))
    }

    types
  }


}
