package ch.ethz.inf.pm.td.parser

import util.parsing.input.Positional

/**
 *
 * Lucas Brutschy
 * Date: 8/24/12
 * Time: 1:32 PM
 *
 */

trait Typed {
  var typeName:TypeName = TypeName("Nothing")
}

case class Script (declarations:List[Declaration]) extends TouchPositional[Script]

sealed trait Scope

sealed trait Declaration extends IdPositional

case class MetaDeclaration(ident:String,value:String)
  extends Declaration
  with TouchPositional[MetaDeclaration]

case class ActionDefinition(ident:String,
                            inParameters:List[Parameter],
                            outParameters:List[Parameter],
                            body:List[Statement],
                            isEvent:Boolean)
  extends Declaration
  with Scope
  with TouchPositional[ActionDefinition]

case class PageDefinition(ident:String,
                          inParameters:List[Parameter],
                          outParameters:List[Parameter],
                          initBody:List[Statement],
                          displayBody:List[Statement])
  extends Declaration
  with Scope
  with TouchPositional[PageDefinition]

case class VariableDefinition(variable:Parameter,
                              flags:Map[String,Any])
  extends Declaration
  with TouchPositional[VariableDefinition]

case class TableDefinition(ident:String,
                           typName:String,
                           keys:List[Parameter],
                           fields:List[Parameter])
  extends Declaration
  with TouchPositional[TableDefinition]

// Library Stuff

case class LibraryDefinition(name:String,
                             pubID:String,
                             usages:List[UsageDeclaration],
                             resolves:List[ResolveBlock])
  extends Declaration
  with TouchPositional[LibraryDefinition]

sealed trait UsageDeclaration extends IdPositional

case class TypeUsage(ident:String)
  extends UsageDeclaration
  with TouchPositional[TypeUsage]

case class ActionUsage(ident:String,
                       inParameters:List[Parameter],
                       outParameters:List[Parameter])
  extends UsageDeclaration
  with TouchPositional[ActionUsage]

case class ResolveBlock(localName:String,
                        libName:String,
                        rules:List[ResolutionRule])
  extends TouchPositional[ResolveBlock]

sealed trait ResolutionRule extends IdPositional

case class TypeResolution(localName:String,libName:TypeName)
  extends ResolutionRule
  with TouchPositional[TypeResolution]

case class ActionResolution(localName:String,libName:String)
  extends ResolutionRule
  with TouchPositional[ActionResolution]

case class Parameter(ident:String,typeName:TypeName)
  extends TouchPositional[Parameter]

case class TypeName(ident:String)
  extends TouchPositional[TypeName] {
  override def toString:String = ident
}

sealed trait Statement extends IdPositional

case class Skip()
  extends Statement
  with TouchPositional[Skip]

case class Box(body:List[Statement]) extends Statement
  with Scope
  with TouchPositional[Box]

case class For(boundLocal: String, upperBound: Expression, body: List[Statement])
  extends Statement
  with Scope
  with TouchPositional[For]

case class If(condition:Expression,thenBody:List[Statement],elseBody:List[Statement])
  extends Statement
  with Scope
  with TouchPositional[If]

case class Foreach(boundLocal: String, collection: Expression, guards: List[Expression], body: List[Statement])
  extends Statement
  with Scope
  with TouchPositional[Foreach]

case class While(condition: Expression, body: List[Statement])
  extends Statement
  with Scope
  with TouchPositional[While]

case class MetaStatement(key: String, value: Any)
  extends Statement
  with TouchPositional[MetaStatement]

case class ExpressionStatement(expr: Expression)
  extends Statement
  with TouchPositional[ExpressionStatement]

case class WhereStatement(expr:Expression,handlers:List[InlineAction])
  extends Statement
  with Scope
  with TouchPositional[WhereStatement]

case class InlineAction(handlerName:String,
                        inParameters:List[Parameter],
                        outParameters:List[Parameter],
                        body:List[Statement])
  extends TouchPositional[InlineAction]

sealed trait Expression extends IdPositional with Typed

case class Access(subject:Expression,property:Identifier,args:List[Expression])
  extends Expression
  with TouchPositional[Access]

case class Literal(typ:TypeName, value:String)
  extends Expression
  with TouchPositional[Literal]

case class SingletonReference(singleton:String,typ:String)
  extends Expression
  with TouchPositional[SingletonReference]

case class LocalReference(ident:String)
  extends Expression
  with TouchPositional[LocalReference]

case class Identifier(ident:String) extends TouchPositional[Identifier] {
  override def toString = ident
}

trait IdPositional extends Positional {

  def getId:Option[String]
  def getPositionAsString:String
  def getPositionDescription:String

}

/**
 * May carry a scala parser like position or an ID
 *
 * Can be copied to another object
 */
trait TouchPositional[T <: TouchPositional[T]] extends IdPositional {

  var id:Option[String] = None

  def copyPos(d:T):T = { pos = d.pos; id = d.id; this.asInstanceOf[T] }

  def setId(newId:String):T = { id = Some(newId); this.asInstanceOf[T] }

  override def getId:Option[String] = id

  override def getPositionAsString:String = {
    id match {
      case Some(x) => x
      case None => pos.toString()
    }
  }

  override def getPositionDescription:String = {
    id match {
      case Some(x) => "at node "+id
      case None => "at line "+pos.line+", column "+pos.column
    }
  }


}
