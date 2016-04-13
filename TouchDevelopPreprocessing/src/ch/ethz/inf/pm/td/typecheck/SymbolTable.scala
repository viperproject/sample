/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.typecheck

import ch.ethz.inf.pm.td.parser._
import scala.collection._
import ch.ethz.inf.pm.td.compiler.{TypeList, CFGGenerator, TouchException}

import scala.util.parsing.input.Position

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


class SymbolTable(script:Script) {

  /** Global fields: Data, Art */
  private val data = new mutable.HashMap[String,TypeName]

  /** Global functions: Actions, Events */
  private val code = new mutable.HashMap[String,Map[List[TypeName],List[TypeName]]]

  /** Local variables */
  private val scopes = new mutable.HashMap[Scope,ScopeSymbolTable]

  /** Library Actions */
  private val libs = new mutable.HashMap[String,Map[String,Map[List[TypeName],List[TypeName]]]]

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

  def resolveUsertypeAccess(typ: TypeName, symbol: String, args: List[TypeName] = Nil): Option[List[TypeName]] = {

    // library function of type lib->f(record,...) can be run as record->f(...) ()
    for ((_, lib) <- libs) {
      lib.get(symbol) match {
        case Some(x) => x.get(typ :: args) match {
          case Some(y) => return Some(y)
          case None => ()
        }
        case None =>
      }
    }

    // local function of type code->f(record,...) can be run as record->f(...)
    code.get(symbol) match {
      case Some(x) => x.get(typ :: args) match {
        case Some(y) => return Some(y)
        case None => ()
      }
      case None =>
    }

    None
  }

  def resolveAccess(typ: TypeName, symbol: String, args: List[TypeName] = Nil, pos: Position): List[TypeName] = {

    typ match {
      case TypeName("code",_,_) => resolveCode(symbol, args, pos)
      case TypeName("♻",_,_) => List(TypeName(CFGGenerator.libraryIdent(symbol)))
      case TypeName("data",_,_) => List(resolveData(symbol, pos))
      case TypeName("art",_,_) => List(resolveData(symbol, pos))
      case _ =>
        if (CFGGenerator.isLibraryIdent(typ.ident)) {
          val lib = CFGGenerator.getLibraryName(typ.ident)
          resolveLib(lib, symbol, args, pos)
        } else {
          resolveUsertypeAccess(typ, symbol, args) match {
            case Some(x) => x
            case None =>
              TypeList.getTypeOrFail(typ).getDeclaration(symbol) match {
                case Some(member) => List(member.returnType.typeName)
                case None =>

                  // Implicit conversion to a reference?
                  TypeList.getTypeOrFail(TypeName("Ref",List(typ))).getDeclaration(symbol) match {

                    case Some(member) =>
                      List(member.returnType.typeName)
                    case None =>
                      throw new TouchException("API method not found: " + typ + "." + symbol + " with arguments " + args, pos)

                  }

              }
          }
        }
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
      case e:NoSuchElementException =>
        throw new TouchException("Action/Event not found: "+action+" with arguments "+types,pos)
    }
  }

  def resolveLib(lib:String, action:String, types:List[TypeName], pos:Position):List[TypeName] = {
    try {
      libs(lib)(action)(types)
    } catch {
      case e:NoSuchElementException => throw new TouchException("Library Action not found: "+lib+"->"+action+" with arguments "+types,pos)
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
