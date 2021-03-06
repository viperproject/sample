/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

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

  var typeName: TypeName = TypeName("Nothing")

  def copyType(d: Typed): this.type = {
    typeName = d.typeName; this
  }

}

trait Scope

case class Script(declarations: List[Declaration], isLibrary: Boolean) extends IdPositional {

  override def toString = PrettyPrinter(this)

}

sealed trait Declaration extends IdPositional with Scope

case class MetaDeclaration(ident: String, value: String)
  extends Declaration
    with IdPositional


case class PageDefinition(ident: String,
    inParameters: List[Parameter],
    outParameters: List[Parameter],
    initBody: List[Statement],
    displayBody: List[Statement],
    isEvent: Boolean,
    isPrivate: Boolean)
  extends Declaration
    with IdPositional

case class ActionDefinition(ident: String,
    inParameters: List[Parameter],
    outParameters: List[Parameter],
    body: List[Statement],
    isEvent: Boolean,
    isPrivate: Boolean)
  extends Declaration
    with IdPositional

case class LibAbstractType(ident: String)
  extends Declaration
    with IdPositional


case class ActionType(ident: String,
    inParameters: List[Parameter],
    outParameters: List[Parameter],
    isPrivate: Boolean)
  extends Declaration
    with IdPositional

case class VariableDefinition(variable: Parameter,
    flags: Map[String, Either[Boolean, String]])
  extends Declaration
    with IdPositional

case class TableDefinition(ident: String,
    typName: String,
    sourceName: Option[String],
    keys: List[Parameter],
    fields: List[Parameter],
    isCloudEnabled: Boolean,
    isPartiallyCloudEnabled: Boolean,
    isPersistent: Boolean,
    isExported: Boolean)
  extends Declaration
    with IdPositional

// Library Stuff

case class LibraryDefinition(name: String,
    libIdentifier: String,
    libIsPublished: Boolean,
    scriptName: String, // name of the script to which the library resolves
    exportedTypes: String,
    exportedTypeDefs: List[Declaration],
    exportedActions: List[ActionUsage],
    resolves: List[ResolveBlock])
  extends Declaration
    with IdPositional

sealed trait UsageDeclaration extends IdPositional

case class ActionUsage(ident: String,
    inParameters: List[Parameter],
    outParameters: List[Parameter])
  extends UsageDeclaration
    with IdPositional

case class ResolveBlock(localName: String,
    libName: String,
    rules: List[ResolutionRule])
  extends IdPositional

sealed trait ResolutionRule extends IdPositional

case class TypeResolution(localName: String, libName: TypeName)
  extends ResolutionRule
    with IdPositional

case class ActionResolution(localName: String, libName: String)
  extends ResolutionRule
    with IdPositional

case class Parameter(ident: String, typeName: TypeName)
  extends IdPositional

object TypeName {

  def parseCode(s: String): TypeName = {
    val TypeNameEmpty = """^TypeName\("([^"]+)"\)$""".r
    val TypeNameRecursive = """^TypeName\("([^"]+)",List\((.*)\)\)$""".r
    s match {
      case TypeNameEmpty(a) => TypeName(a)
      case TypeNameRecursive(a, b) => TypeName(a, b.split(",").map(parseCode).toList)
    }
  }

}

case class TypeName(ident: String, arguments: List[TypeName] = Nil, isSingleton: Boolean = false, isUserDefined: Boolean = false)
  extends IdPositional with Serializable {
  override lazy val toString: String = (arguments ::: List(ident)).mkString(" ")
}

sealed trait Statement extends IdPositional with Scope {
  def hasSubExpression: Boolean = true

  def hasSubStatement: Boolean = true
}

sealed trait NoSubStatement extends Statement {
  override def hasSubStatement: Boolean = false
}

sealed trait NoSubExpression extends NoSubStatement {
  override def hasSubExpression: Boolean = false
}

case class Skip()
  extends Statement
    with IdPositional
    with NoSubExpression

case class Box(body: List[Statement])
  extends Statement
    with IdPositional

case class For(boundLocal: String, upperBound: Expression, body: List[Statement])
  extends Statement
    with IdPositional

case class If(condition: Expression, thenBody: List[Statement], elseBody: List[Statement])
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
    with NoSubExpression

case class ExpressionStatement(expr: Expression)
  extends Statement
    with IdPositional
    with NoSubStatement

case class WhereStatement(expr: Expression, handlers: List[InlineAction], optionalParameters: List[OptionalParameter])
  extends Statement
    with IdPositional

case class Show(expr: Expression)
  extends Statement
    with IdPositional
    with NoSubStatement

case class Break()
  extends Statement
    with IdPositional
    with NoSubExpression

case class Return(expr: Expression)
  extends Statement
    with IdPositional
    with NoSubStatement

case class Continue()
  extends Statement
    with IdPositional
    with NoSubExpression

case class OptionalParameter(name: String, expr: Expression) extends IdPositional

case class InlineAction(handlerName: String,
    inParameters: List[Parameter],
    outParameters: List[Parameter],
    body: List[Statement],
    typ: TypeName)
  extends IdPositional

sealed trait Expression extends IdPositional with Typed

case class Access(subject: Expression, property: Identifier, args: List[Expression])
  extends Expression
    with IdPositional

case class Literal(typ: TypeName, value: String)
  extends Expression
    with IdPositional

case class SingletonReference(singleton: String, typ: String)
  extends Expression
    with IdPositional

case class LocalReference(ident: String)
  extends Expression
    with IdPositional

// ident usually empty
// used for optional arguments
case class Placeholder(typ: TypeName)
  extends Expression
    with IdPositional

case class Identifier(ident: String) extends IdPositional {
  override def toString = ident
}

/**
  * May carry a scala parser like position or an ID
  *
  * Can be copied to another object
  */
trait IdPositional extends Positional {

  var customIdComponents: List[String] = Nil

  def copyPos(d: IdPositional): this.type = {
    pos = d.pos; customIdComponents = d.customIdComponents; this
  }

  def setId(newId: String): this.type = {
    customIdComponents = List(newId); this
  }

  def appendIdComponent(suffix: String): this.type = {
    customIdComponents = customIdComponents :+ suffix
    this
  }

  def appendIdComponents(suffix: List[String]): this.type = {
    customIdComponents = customIdComponents ::: suffix
    this
  }

  def getIdComponents: List[String] = customIdComponents

  def getPositionDescription: String = {
    val customIds = customIdComponents.mkString("_")

    pos match {
      case NoPosition =>
        s"at id $customIds"
      case _ =>
        val str = s"at line ${pos.line}, column ${pos.column}"
        if (customIds.isEmpty) str else s"$str, id $customIds"
    }
  }

}
