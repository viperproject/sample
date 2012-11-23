package ch.ethz.inf.pm.td.symbols

import ch.ethz.inf.pm.td.stdlib._
import ch.ethz.inf.pm.td.parser._
import ch.ethz.inf.pm.td.compiler.TouchException

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

  def processScript(script: Script):SymbolTable = {
    val symbolTable = new SymbolTable(script) with StdLib with ObsoleteStdLib
    processScript(script,symbolTable)
    symbolTable
  }

  def processScript(script: Script, symbolTable:SymbolTable) = {
    for (declaration <- script.declarations) addGlobals(script, symbolTable, declaration)
    for (declaration <- script.declarations) typeLocals(script, symbolTable, declaration)
  }


  def addGlobals(scope: Script, st:SymbolTable, thing:Declaration) {
    thing match {
      case a@ActionDefinition(name, inParameters, outParameters, body,isEvent) =>
        st.addAction(name,inParameters,outParameters)
      case VariableDefinition(Parameter(name,kind), flags) =>
        st.addGlobalData(name,kind)
      case TableDefinition(ident,typeName,keys,fields) =>
        for (field <- fields) {
          val fieldType = field.typeName.toString
          val noFieldType = fieldType.replace("_field","")
          if (noFieldType == "Number") st.addUserType(fieldType,GenericTypes.gNumberField(fieldType))
            else st.addUserType(fieldType,GenericTypes.gField(fieldType,noFieldType))
        }

        val fieldMembers = fields map {case Parameter(x,typ) => Member(x,typ)}

        typeName match {
          case "Object" =>
            st.addUserType(ident,GenericTypes.gObject(ident,fieldMembers))
            st.addUserType(ident+"_Collection",GenericTypes.gMutableCollection(ident+"_Collection",ident))
            st.addUserType(ident+"_Constructor",List(Member("create",ident),Member("create_collection",ident+"_Collection")))
            st.addUserSingleton("records",List(Member(ident,ident+"_Constructor")))
          case "Table" =>
            st.addUserType(ident,GenericTypes.gRow(ident,fieldMembers))
            st.addUserType(ident+"_Table",GenericTypes.gTable(ident+"_Table",ident))
            st.addUserSingleton("records",List(Member(ident+"_table",ident+"_Table")))
          case "Index" =>
            val keyMembers = keys map {case Parameter(x,typ) => Member(x,typ)}
            val keyTypes = keys map {case Parameter(_,typ) => typ.toString}
            val fieldAndKeyMembers = fieldMembers ::: keyMembers
            st.addUserType(ident,GenericTypes.gIndexMember(ident,fieldAndKeyMembers))
            st.addUserType(ident+"_Index", GenericTypes.gIndex(ident+"_Index",keyTypes, ident))
            st.addUserSingleton("records",List(Member(ident+"_index",ident+"_Index")))
          case "Decorator" =>
            if (keys.size != 1) throw TouchException("Decorators must have exactly one entry",thing.pos)
            val decoratedType = keys.head.typeName.toString
            val keyMembers = keys map {case Parameter(x,typ) => Member(x,typ)}
            val fieldAndKeyMembers = fieldMembers ::: keyMembers
            st.addUserType(ident,GenericTypes.gIndexMember(ident,fieldAndKeyMembers))
            st.addUserType(decoratedType+"_Decorator", GenericTypes.gIndex(decoratedType+"_Decorator",List(decoratedType), ident))
            st.addUserSingleton("records",List(Member(decoratedType+"_decorator",decoratedType+"_Decorator")))
          case _ => throw TouchException("Table type "+typeName+" not supported",thing.pos)

        }
      case LibraryDefinition(libName,pub,usages,resolves) =>
        for (usage <- usages) { usage match {
          case ActionUsage(name,in,out) => st.addLibAction(libName,name,in,out)
          case TypeUsage(ident) => throw TouchException("Type usages are unsupported.")
        }}
      case _ => Unit
    }
  }

  def typeLocals(scope: Script, st:SymbolTable, thing:Declaration) {
    thing match {
      case a@ActionDefinition(name, inParameters, outParameters, body,isEvent) =>
        st(a) = ScopeSymbolTable(a,null,Map.empty) ++ inParameters ++ outParameters
        for (smt <- body) processStatement(a,st,smt)
      case _ => Unit
    }
  }

  def processStatement(scope: Scope, st:SymbolTable, statement: Statement) {

    def handleSingleAssignment (a:AssignStatement,variable:LValue,typ:TypeName) {
    variable match {
      case l@LocalReference(ident) =>
        st.tryResolveLocal(scope,ident) match {
          case Some(x) =>
            if (x != typ) throw TouchException("Assignment to wrong type. Expected: "+x+", Found: "+typ,statement.pos)
          case None =>
            a.isVariableDeclaration = true
            st(scope) = st(scope) + (ident -> typ)
        }
      case g@GlobalReference(ident) =>
        val expType = st.resolveData(ident,g.pos)
        if( expType != typ ) throw TouchException("Assignment to wrong type. Expected: "+expType+", Found: "+typ,statement.pos)
      }
      variable.typeName = typ
    }

    statement match {
      case s@While(cond, body) =>
        st(s) = ScopeSymbolTable(s,scope,Map.empty)
        processExpression(s,st,cond)
        for (smt <- body) processStatement(s,st,smt)
      case s@If(cond, thenBody, elseBody) =>
        // FIXME: Currently then and else scopes are joined
        st(s) = ScopeSymbolTable(s,scope,Map.empty)
        processExpression(s,st,cond)
        for (smt <- thenBody) processStatement(s,st,smt)
        for (smt <- elseBody) processStatement(s,st,smt)
      case ExpressionStatement(expr) =>
        processMultiValExpression(scope,st,expr)
      case a@AssignStatement(left,right) =>
        val types = processMultiValExpression(scope,st,right)
        if (left.length != types.length) throw TouchException("Typing: Invalid number of lvalues",statement.pos)
        for (i <- types.indices) handleSingleAssignment(a,left(i),types(i))
      case _ => Unit
    }
  }

  def processExpression(scope: Scope, st:SymbolTable, expr:Expression):TypeName = {

    def is(typ:TypeName):TypeName = { expr.typeName = typ; typ }

    expr match {
      case Access(subject,property,args) =>
        val types = for(arg <- args) yield processExpression(scope,st,arg)
        subject match {
          case s@SingletonReference("code") =>
            val retTypes = st.resolveCode(property,types,s.pos)
            if (retTypes.length > 1) throw TouchException("Multiple return values "+retTypes+" in non-assignment expression",expr.pos)
            else if (retTypes.length < 1) is(TypeName("Nothing"))
            else is(retTypes.head)
          case l@LibraryReference(lib) =>
            val retTypes = st.resolveLib(lib,property,types,l.pos)
            // subject.typeName = TypeName("__script_"+lib) // TODO
            if (retTypes.length > 1) throw TouchException("Multiple return values "+retTypes+" in non-assignment expression",expr.pos)
            else if (retTypes.length < 1) is(TypeName("Nothing"))
            else is(retTypes.head)
          case _ =>
            val subtype = processExpression(scope,st,subject)
            is(st.resolveAccess(subtype,property,types))
        }
      case l@LocalReference(ident) => is(st.resolveLocal(scope,ident,l.pos))
      case g@GlobalReference(ident) => is(st.resolveData(ident,g.pos))
      case SingletonReference(singleton) => is(TypeName(singleton))
      case l@LibraryReference(property) => throw TouchException("library reference not expected here",expr.pos)
      case Literal(typ,_) => is(typ)
    }
  }

  def processMultiValExpression(scope: Scope, st:SymbolTable, expr:Expression):List[TypeName] = {
    expr match {
      case Access(subject@SingletonReference("code"),action,args) =>
        val types = for(arg <- args) yield processExpression(scope,st,arg)
        subject.typeName = TypeName("code")
        st.resolveCode(action,types,subject.pos)
      case Access(l@LibraryReference(lib),action,args) =>
        val types = for(arg <- args) yield processExpression(scope,st,arg)
        st.resolveLib(lib,action,types,l.pos)
      case _ => List(processExpression(scope,st,expr))
    }
  }


}
