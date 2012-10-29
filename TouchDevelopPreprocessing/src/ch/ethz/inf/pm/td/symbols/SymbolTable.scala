package ch.ethz.inf.pm.td.symbols

import ch.ethz.inf.pm.td.parser._
import scala.collection._
import ch.ethz.inf.pm.td.compiler.TouchException
import util.parsing.input.Position

/**
 *
 * Lucas Brutschy
 * Date: 8/7/12
 * Time: 3:41 PM
 *
 */

case class Singleton (name:String,members:List[Member])
case class Type (name:String,members:List[Member])

object Member {
  def apply(name:String, retType:String): Member = Member(name,Nil,TypeName(retType))
  def apply(name:String, retType:TypeName): Member = Member(name,Nil,retType)
  def apply(name:String, argTypes:List[String], retType:String): Member = Member(name,argTypes map (TypeName(_)),TypeName(retType))
}

case class Member (name:String,argsTypes:List[TypeName],retType:TypeName)


abstract class AbstractSymbolTable {

  protected var types = new mutable.HashMap[TypeName,Map[String,Member]]

  def addSingleton (name:String,members:List[Member]) {
    val objName = TypeName(name)
    types(objName) = if (types.contains(objName)) types(objName) else immutable.Map.empty[String,Member]
    for (mem <- members) types(objName) = types(objName) + (mem.name -> mem)
  }

  def addType (name:String,members:List[Member]) {
    val typeName = TypeName(name)
    types(typeName) = if (types.contains(typeName)) types(typeName) else immutable.Map.empty[String,Member]
    for (mem <- members) types(typeName) = types(typeName) + (mem.name -> mem)
  }

  def addSingletonMember (a1:String,a2:String,a3:Member) {
    types(TypeName(a1)) = types(TypeName(a1)) + (a2 -> a3)
  }

  def addTypeMember (a1:TypeName,a2:String,a3:Member) {
    types(a1) = types(a1) + (a2 -> a3)
  }

  def resolveAccess(typ:TypeName, symbol:String, args:List[TypeName]=Nil):TypeName = {
    try {
      types(typ)(symbol).retType
    } catch {
      case e:NoSuchElementException => throw new TouchException("API method not found: "+typ+"."+symbol+" with arguments "+args)
    }
  }

}

class SymbolTable(script:Script) extends AbstractSymbolTable {

  /** Global fields: Data, Art */
  private var data = new mutable.HashMap[String,TypeName]

  /** Global functions: Actions, Events */
  private var code = new mutable.HashMap[String,Map[List[TypeName],List[TypeName]]]

  /** Local variables */
  private var scopes = new mutable.HashMap[Scope,ScopeSymbolTable]

  /** Library Actions */
  private var libs = new mutable.HashMap[String,Map[String,Map[List[TypeName],List[TypeName]]]]

  /** User defined types */
  private var usertypes = new mutable.HashMap[TypeName,Map[String,Member]]

  def addUserSingleton (name:String,members:List[Member]) {
    val objName = TypeName(name)
    usertypes(objName) = if (usertypes.contains(objName)) usertypes(objName) else immutable.Map.empty[String,Member]
    for (mem <- members) usertypes(objName) = usertypes(objName) + (mem.name -> mem)
  }

  def addUserType (name:String,members:List[Member]) {
    val typeName = TypeName(name)
    usertypes(typeName) = if (usertypes.contains(typeName)) usertypes(typeName) else immutable.Map.empty[String,Member]
    for (mem <- members) usertypes(typeName) = usertypes(typeName) + (mem.name -> mem)
  }

  def addLibAction (a1:String,a2:String,a3:List[Parameter],a4:List[Parameter]) {
    val libSym = if(libs.contains(a1)) libs(a1) else Map.empty[String,Map[List[TypeName],List[TypeName]]]
    val actionSym = if (libSym.contains(a2)) libSym(a2) else Map.empty[List[TypeName],List[TypeName]]
    libs(a1) = libSym + (a2 -> (actionSym + ((a3 map (_.typeName)) -> (a4 map (_.typeName)))))
  }

  def addAction (a1:String,a2:List[Parameter],a3:List[Parameter]) {
    code(a1) =
      (if(code.contains(a1)) code(a1) else Map.empty[List[TypeName],List[TypeName]]) +
        ((a2 map (_.typeName)) -> (a3 map (_.typeName)))
  }

  def addGlobalData (a1:String,a2:TypeName) {
    data(a1) = a2
  }

  def apply(s:Scope):ScopeSymbolTable = {
    scopes(s)
  }

  def update(s:Scope, st:ScopeSymbolTable) {
    scopes(s) = st
  }



  def resolveUsertypeAccess(typ:TypeName, symbol:String, args:List[TypeName]=Nil):Option[TypeName] = {
    try {
      Some(usertypes(typ)(symbol).retType)
    } catch {
      case e:NoSuchElementException => None
    }
  }

  override def resolveAccess(typ:TypeName, symbol:String, args:List[TypeName]=Nil):TypeName = {
    resolveUsertypeAccess(typ,symbol,args) match {
      case Some(x) => x
      case None => super.resolveAccess(typ,symbol,args)
    }
  }

  def resolveData(symbol:String, pos:Position):TypeName = {
    try {
      data(symbol)
    } catch {
      case e:NoSuchElementException => throw new TouchException("Data not found: "+symbol,pos)
    }
  }

  def tryResolveLocal(scope:Scope, symbol:String):Option[TypeName] = {
    scopes(scope) match {
      case st:ScopeSymbolTable =>
        st.getType(symbol) match {
          case None => if (st.parent != null) tryResolveLocal(st.parent,symbol) else None
          case x => x
        }
      case _ => None
    }
  }

  def resolveLocal(scope:Scope, symbol:String, pos:Position):TypeName = {
    tryResolveLocal(scope,symbol) match {
      case Some(x) => x
      case None => throw new TouchException("Local variable not found: "+symbol, pos)
    }
  }

  def resolveCode(action:String, types:List[TypeName], pos:Position):List[TypeName] = {
    try {
      code(action)(types)
    } catch {
      case e:NoSuchElementException => throw new TouchException("Action/Event not found: "+action+" with arguments "+types,pos)
    }
  }

  def resolveLib(lib:String, action:String, types:List[TypeName], pos:Position):List[TypeName] = {
    try {
      libs(lib)(action)(types)
    } catch {
      case e:NoSuchElementException => throw new TouchException("Library Action not found: "+action+" with arguments "+types,pos)
    }
  }

}

case class ScopeSymbolTable(scope:Scope,parent:Scope,variables:Map[String,TypeName]) {

  def ++ (s:ScopeSymbolTable):ScopeSymbolTable = ScopeSymbolTable(scope,parent,variables ++ s.variables)
  def ++ (l:List[Parameter]):ScopeSymbolTable = ScopeSymbolTable(scope,parent,(variables /: l)( (x,y) => x + (y.ident -> y.typeName)))
  def + (l:Parameter):ScopeSymbolTable = ScopeSymbolTable(scope,parent,variables + (l.ident -> l.typeName))
  def + (t:(String,TypeName)):ScopeSymbolTable = ScopeSymbolTable(scope,parent,variables + t)
  def getType(symbol:String):Option[TypeName] = variables.get(symbol)

}
