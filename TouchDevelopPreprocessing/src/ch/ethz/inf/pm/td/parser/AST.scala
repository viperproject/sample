package ch.ethz.inf.pm.td.parser

import util.parsing.input.Positional

/**
 *
 * Lucas Brutschy
 * Date: 8/24/12
 * Time: 1:32 PM
 *
 */

trait CopyablePositional[T <: CopyablePositional[T]] extends Positional {
  def copyPos(d:T):T = { pos = d.pos; this.asInstanceOf[T] }
}

trait Typed {
  var typeName:TypeName = TypeName("Nothing")
}

case class Script (declarations:List[Declaration]) extends CopyablePositional[Script]

sealed trait Scope
sealed trait Declaration extends Positional
case class MetaDeclaration(ident:String,value:String) extends Declaration
case class ActionDefinition(ident:String,inParameters:List[Parameter],outParameters:List[Parameter],body:List[Statement],
                            isEvent:Boolean) extends Declaration with Scope with CopyablePositional[ActionDefinition]
case class VariableDefinition(variable:Parameter,flags:Map[String,Any]) extends Declaration
case class TableDefinition(ident:String,typName:String,keys:List[Parameter],fields:List[Parameter]) extends Declaration

// Library Stuff
case class LibraryDefinition(name:String,pubID:String,usages:List[UsageDeclaration],resolves:List[ResolveBlock]) extends Declaration
sealed trait UsageDeclaration extends CopyablePositional[UsageDeclaration]
case class TypeUsage(ident:String) extends UsageDeclaration
case class ActionUsage(ident:String,inParameters:List[Parameter],outParameters:List[Parameter]) extends UsageDeclaration
case class ResolveBlock(localName:String,lib:LibraryReference,rules:List[ResolutionRule]) extends CopyablePositional[ResolveBlock]
sealed trait ResolutionRule extends CopyablePositional[ResolutionRule]
case class TypeResolution(localName:String,libName:TypeName) extends ResolutionRule
case class ActionResolution(localName:String,lib:LibraryReference,libName:String) extends ResolutionRule

case class Parameter(ident:String,typeName:TypeName) extends CopyablePositional[Parameter]
case class TypeName(ident:String,library:Option[LibraryReference]=None) extends CopyablePositional[TypeName] {
  override def toString:String = {
    library match {
      case Some(LibraryReference(lib)) => "libs."+lib+"."+ident
      case None => ident
    }
  }
}

sealed trait Statement extends CopyablePositional[Statement]
case class Skip() extends Statement
case class Box(body:List[Statement]) extends Statement with Scope
case class For(boundLocal: String, upperBound: Expression, body: List[Statement]) extends Statement with Scope
case class If(condition:Expression,thenBody:List[Statement],elseBody:List[Statement]) extends Statement with Scope
case class Foreach(boundLocal: String, collection: Expression, guards: List[Expression], body: List[Statement]) extends Statement with Scope
case class While(condition: Expression, body: List[Statement]) extends Statement with Scope
case class MetaStatement(key: String, value: Any) extends Statement
case class ExpressionStatement(expr: Expression) extends Statement
case class WhereStatement(expr:Expression,handlerName:String,parameters:List[Parameter],body:List[Statement]) extends Statement with Scope
case class AssignStatement(left:List[LValue],right:Expression) extends Statement {
  var isVariableDeclaration:Boolean = false
}

sealed trait Expression extends CopyablePositional[Expression] with Typed

case class Access(subject:Expression,property:Identifier,args:List[Expression]) extends Expression
case class LibraryReference(ident:String) extends Expression
case class Literal(typ:TypeName, value:String) extends Expression
case class SingletonReference(singleton:String) extends Expression

sealed trait LValue extends Expression
case class LocalReference(ident:String) extends LValue
case class GlobalReference(ident:String) extends LValue

case class Identifier(ident:String) extends CopyablePositional[Identifier]
