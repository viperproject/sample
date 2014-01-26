package ch.ethz.inf.pm.td.parser

import scala.util.parsing.input.{NoPosition, Positional}

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

trait Scope

case class Script (declarations:List[Declaration], isLibrary:Boolean) extends IdPositional

sealed trait Declaration extends IdPositional with Scope

case class MetaDeclaration(ident:String,value:String)
  extends Declaration
  with IdPositional

case class ActionDefinition(ident:String,
                            inParameters:List[Parameter],
                            outParameters:List[Parameter],
                            body:List[Statement],
                            isEvent:Boolean,
                            isPrivate:Boolean)
  extends Declaration
  with IdPositional

case class PageDefinition(ident:String,
                          inParameters:List[Parameter],
                          outParameters:List[Parameter],
                          initBody:List[Statement],
                          displayBody:List[Statement],
                          isPrivate:Boolean)
  extends Declaration
  with IdPositional

case class VariableDefinition(variable:Parameter,
                              flags:Map[String,Any])
  extends Declaration
  with IdPositional

case class TableDefinition(ident:String,
                           typName:String,
                           keys:List[Parameter],
                           fields:List[Parameter])
  extends Declaration
  with IdPositional

// Library Stuff

case class LibraryDefinition(name:String,
                             pubID:String,
                             usages:List[UsageDeclaration],
                             resolves:List[ResolveBlock])
  extends Declaration
  with IdPositional

sealed trait UsageDeclaration extends IdPositional

case class TypeUsage(ident:String)
  extends UsageDeclaration
  with IdPositional

case class ActionUsage(ident:String,
                       inParameters:List[Parameter],
                       outParameters:List[Parameter])
  extends UsageDeclaration
  with IdPositional

case class ResolveBlock(localName:String,
                        libName:String,
                        rules:List[ResolutionRule])
  extends IdPositional

sealed trait ResolutionRule extends IdPositional

case class TypeResolution(localName:String,libName:TypeName)
  extends ResolutionRule
  with IdPositional

case class ActionResolution(localName:String,libName:String)
  extends ResolutionRule
  with IdPositional

case class Parameter(ident:String,typeName:TypeName)
  extends IdPositional

case class TypeName(ident:String)
  extends IdPositional {
  override def toString:String = ident
}

sealed trait Statement extends IdPositional with Scope

case class Skip()
  extends Statement
  with IdPositional

case class Box(body:List[Statement])
  extends Statement
  with IdPositional

case class For(boundLocal: String, upperBound: Expression, body: List[Statement])
  extends Statement
  with IdPositional

case class If(condition:Expression,thenBody:List[Statement],elseBody:List[Statement])
  extends Statement
  with IdPositional

case class Foreach(boundLocal: String, collection: Expression, guards: List[Expression], body: List[Statement])
  extends Statement
  with IdPositional

case class While(condition: Expression, body: List[Statement])
  extends Statement
  with IdPositional

case class MetaStatement(key: String, value: Any)
  extends Statement
  with IdPositional

case class ExpressionStatement(expr: Expression)
  extends Statement
  with IdPositional

case class WhereStatement(expr:Expression,handlers:List[InlineAction])
  extends Statement
  with IdPositional

case class InlineAction(handlerName:String,
                        inParameters:List[Parameter],
                        outParameters:List[Parameter],
                        body:List[Statement])
  extends IdPositional

sealed trait Expression extends IdPositional with Typed

case class Access(subject:Expression,property:Identifier,args:List[Expression])
  extends Expression
  with IdPositional

case class Literal(typ:TypeName, value:String)
  extends Expression
  with IdPositional

case class SingletonReference(singleton:String,typ:String)
  extends Expression
  with IdPositional

case class LocalReference(ident:String)
  extends Expression
  with IdPositional

case class Identifier(ident:String) extends IdPositional {
  override def toString = ident
}

/**
 * May carry a scala parser like position or an ID
 *
 * Can be copied to another object
 */
trait IdPositional extends Positional {

  var id:Option[String] = None

  def copyPos(d: IdPositional): this.type = { pos = d.pos; id = d.id; this }

  def setId(newId:String): this.type = { id = Some(newId); this }

  def appendId(suffix: String): this.type = {
    id = Some(id.getOrElse("") + suffix)
    this
  }

  def setOptionalId(x:Option[String]) { id = x }

  def getId:Option[String] = id

  def getPositionAsString:String = {
    id match {
      case Some(x) => if (pos == NoPosition) x else pos.toString + x
      case None => pos.toString
    }
  }

  def getPositionDescription:String = {
    id match {
      case Some(x) => "at node "+id
      case None => "at line "+pos.line+", column "+pos.column
    }
  }

}
