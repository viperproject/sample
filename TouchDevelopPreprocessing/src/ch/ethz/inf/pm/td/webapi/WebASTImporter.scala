/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.webapi

import ch.ethz.inf.pm.td.compiler.TouchException
import ch.ethz.inf.pm.td.parser._
import ch.ethz.inf.pm.td.webapi.WebAST._
import net.liftweb.json.JsonAST.{JArray, JObject, JString}
import net.liftweb.json._

/**
  *
  * Imports the JSON formatted AST of a script from the TouchDevelop web-interface
  *
  * Created by Lucas Brutschy
  *
  */
object WebASTImporter {

  implicit val formats = new DefaultFormats {
    override val typeHintFieldName = "nodeType"
    override val typeHints = WebAstTypeHints(List(classOf[JOperator], classOf[JPropertyRef], classOf[JStringLiteral],
      classOf[JBooleanLiteral], classOf[JNumberLiteral], classOf[JLocalRef], classOf[JPlaceholder], classOf[JSingletonRef],
      classOf[JExprHolder], classOf[JComment], classOf[JFor], classOf[JForeach], classOf[JWhere], classOf[JWhile],
      classOf[JIf], classOf[JBoxed], classOf[JExprStmt], classOf[JInlineActions], classOf[JInlineAction],
      classOf[JAction], classOf[JPage], classOf[JEvent], classOf[JLibAction], classOf[JArt], classOf[JData], classOf[JLibrary],
      classOf[JTypeBinding], classOf[JActionBinding], classOf[JResolveClause], classOf[JRecord], classOf[JRecordField],
      classOf[JRecordKey], classOf[JLocalDef], classOf[JApp], classOf[JPropertyParameter], classOf[JProperty],
      classOf[JTypeDef], classOf[JApis], classOf[JCall], classOf[JActionType], classOf[JOptionalParameter],
      classOf[JLibActionType], classOf[JLibAbstractType], classOf[JBreak], classOf[JReturn], classOf[JLibRecordType],
      classOf[JShow], classOf[JContinue])
    )
  }

  def queryAndConvert(pubID: String): Option[Script] = {
    query(pubID) map convert
  }

  def queryAndConvertBoth(pubID: String): Option[(Script, Option[JApp])] = {
    query(pubID) match {
      case Some(script) => Some(convert(script), Some(script))
      case None => None
    }
  }

  def query(pubID: String): Option[JApp] = {
    val url = ScriptQuery.webastURLfromPubID(pubID)
    val string = URLFetcher.fetchFile(url)
    val json = parseOpt(string)
    json map {
      _.asInstanceOf[JObject].extract[JApp]
    }
  }

  def parseAST(string: String): Option[JApp] = {
    val json = parseOpt(string)
    if (json.isDefined) {
      Some(json.get.asInstanceOf[JObject].extract[JApp])
    } else {
      None
    }
  }

  def convertFromString(string: String): Option[Script] = {
    val json = parseOpt(string)
    json match {
      case Some(x) => Some(convert(x.extract[JApp]))
      case None => None
    }
  }

  def convertFromStringBoth(string: String): Option[(Script, Option[JApp])] = {
    val json = parseOpt(string)
    json match {
      case Some(x) =>
        val japp = x.extract[JApp]
        Some(convert(japp), Some(japp))
      case None => None
    }
  }

  def convert(jAST: JApp): Script = {
    Script(jAST.decls map convert, jAST.isLibrary).setId(jAST.rootId)
  }

  def convert(jDecl: JDecl): Declaration = {
    jDecl match {
      case JArt(id, name, comment, typ, isReadonly, _, _, url, _) =>
        VariableDefinition(Parameter(name, makeTypeName(typ).setId(id)).setId(id), Map(
          "readonly" -> Left(isReadonly),
          "is_resource" -> Left(true),
          "transient" -> Left(false),
          "cloudenabled" -> Left(false)
        )).setId(id)
      case JData(id, name, comment, typ, isReadonly, isCloudEnabled, isTransient) =>
        VariableDefinition(Parameter(name, makeTypeName(typ).setId(id)).setId(id), Map(
          "readonly" -> Left(isReadonly),
          "is_resource" -> Left(false),
          "transient" -> Left(isTransient),
          "cloudenabled" -> Left(isCloudEnabled)
        )).setId(id)
      case JPage(id, name, inParameters, outParameters, isPrivate, _, _, _, _, _, initBody, displayBody, _, _, hasModelParameter) =>
        // TODO: This could have a loop for the display code, but since its side-effect free, it should be fine.
        hasModelParameter match {
          case Some(true) =>
            val modParam = convert(inParameters.head)
            val records = SingletonReference("records", "records").setId(id)
            val paramRecord = Access(records, Identifier(modParam.typeName.toString).setId(id), Nil).setId(id)
            val createRecord = Access(paramRecord, Identifier("create").setId(id), Nil).setId(id)
            val assignment = Access(LocalReference(modParam.ident).setId(id), Identifier(":=").setId(id), List(createRecord)).setId(id)
            val initCode = ExpressionStatement(assignment).setId(id)
            PageDefinition(name, inParameters.tail map convert, outParameters map convert, initCode :: convert(initBody), convert(displayBody), isEvent = false, isPrivate).setId(id)
          case _ =>
            PageDefinition(name, inParameters map convert, outParameters map convert, convert(initBody), convert(displayBody), isEvent = false, isPrivate).setId(id)
        }
      case JEvent(id, name, inParameters, outParameters, isPrivate, _, _, _, _, _, eventName, eventVariableId, body) =>
        ActionDefinition(name, inParameters map convert, outParameters map convert, convert(body), isEvent = true, isPrivate = isPrivate).setId(id)
      case JLibrary(id, name, libIdentifier, libIsPublished, scriptName, exportedTypes, exportedTypeDefs, exportedActions, resolveClauses) =>
        LibraryDefinition(name, libIdentifier, libIsPublished, scriptName, exportedTypes, exportedTypeDefs map convert, exportedActions map convert, resolveClauses map convert).setId(id)
      case JRecord(id, name, sourceName, comment, category, isCloudEnabled, isCloudPartiallyEnabled, isPersistent, isExported, keys, fields) =>
        TableDefinition(name, category, sourceName, keys map convert, fields map convert, isCloudEnabled, isCloudPartiallyEnabled, isPersistent, isExported).setId(id)
      case JAction(id, name, inParameters, outParameters, isPrivate, _, _, _, _, _, body) =>
        ActionDefinition(name, inParameters map convert, outParameters map convert, convert(body), isEvent = false, isPrivate = isPrivate).setId(id)
      case JActionType(id, name, inParameters, outParameters, isPrivate, _, _, _, _, _) =>
        ActionType(name, inParameters map convert, outParameters map convert, isPrivate = isPrivate).setId(id)
      case JLibActionType(id, name, inParameters, outParameters, isPrivate, _, _, _, _, _) =>
        ActionType(name, inParameters map convert, outParameters map convert, isPrivate = isPrivate).setId(id)
      case JLibAbstractType(id: String, name: String) =>
        LibAbstractType(name).setId(id)
      case JLibRecordType(id, name, sourceName, comment, category, isCloudEnabled, isCloudPartiallyEnabled, isPersistent, isExported, keys, fields) =>
        TableDefinition(name, category, sourceName, keys map convert, fields map convert, isCloudEnabled, isCloudPartiallyEnabled, isPersistent, isExported).setId(id)
    }
  }

  def convert(jResolve: JResolveClause): ResolveBlock = {
    ResolveBlock(jResolve.name, jResolve.defaultLibId,
      (jResolve.withActions map convert) ::: (jResolve.withTypes map convert)).setId(jResolve.id)
  }

  def convert(jLocalDef: JLocalDef): Parameter = {
    Parameter(jLocalDef.name, makeTypeName(jLocalDef.`type`).setId(jLocalDef.id)).setId(jLocalDef.id)
  }

  def convert(stmts: List[JStmt]): List[Statement] = {
    // The new version of the JSON AST has an awkward representation of elseIfs
    // as seperate if like statements IF(BLA) THEN A ELSE SKIP; ELSEIF(BLA2) THEN B ELSE C
    // here, we convert it to IF (BLA) THEN A ELSE { IF (BLA 2) THEN B ELSE C }
    val noElseIfs = stmts.foldRight(List.empty[JStmt])({
      (x: JStmt, y: List[JStmt]) =>
        y match {
          case JIf(id2, cond2, then2, els2, isElseIf2) :: xs if isElseIf2 =>
            x match {
              case JIf(id, cond, thn, els, isElseIf) =>
                JIf(id, cond, thn, els ::: List(JIf(id2, cond2, then2, els2, isElseIf = false)), isElseIf) :: xs
              case _ =>
                throw TouchException("not reachable")
            }
          case _ => x :: y
        }
    })

    noElseIfs map convert
  }

  def convert(jStatement: JStmt): Statement = {
    jStatement match {
      case JComment(id, text) =>
        Skip().setId(id)
      case JFor(id, index, bound, body) =>
        For(index.name, convert(bound), convert(body)).setId(id)
      case JForeach(id, iterator, collection, conditions, body) =>
        Foreach(iterator.name, convert(collection), conditions map convert, convert(body)).setId(id)
      case JWhile(id, condition, body) =>
        While(convert(condition), convert(body)).setId(id)
      case JIf(id, condition, thenBody, elseBody, _) =>
        // We prune if(false) here already, as they are not typechecked!!
        condition match {
          case JExprHolder(_, _, JBooleanLiteral(_, false), _) =>
            Skip().setId(id)
          case _ =>
            If(convert(condition), convert(thenBody), convert(elseBody)).setId(id)
        }
      case JBoxed(id, body) =>
        Box(convert(body)).setId(id)
      case JExprStmt(id, expr) =>
        expr.tree match {
          case JBreak(id1) =>
            Break().setId(id1)
          case JContinue(id1) =>
            Continue().setId(id1)
          case JReturn(id1, expr1) =>
            Return(convert(expr1)).setId(id1)
          case JShow(id1, expr1) =>
            Show(convert(expr1)).setId(id1)
          case _ =>
            ExpressionStatement(convert(expr)).setId(id)
        }
      case JInlineActions(id, expr, actions) =>
        WhereStatement(convert(expr), actions collect { case x: JInlineAction => x } map convert,
          actions collect { case x: JOptionalParameter => x } map convert).setId(id)
    }
  }

  def convert(jExpression: JExprHolder): Expression = {
    convert(jExpression.tree)
  }

  def convert(jExpression: JExpr): Expression = {
    jExpression match {
      case JStringLiteral(id, value) =>
        Literal(makeTypeName("String").setId(id), value).setId(id)
      case JBooleanLiteral(id, value) =>
        Literal(makeTypeName("Boolean").setId(id), value.toString).setId(id)
      case JNumberLiteral(id, value) =>
        Literal(makeTypeName("Number").setId(id), value.toString).setId(id)
      case JLocalRef(id, name, localId) =>
        LocalReference(name).setId(id)
      case JPlaceholder(id, name, typ) =>
        Placeholder(makeTypeName(typ)).setId(id)
      case JSingletonRef(id, name, typ) =>
        SingletonReference(name, typ).setId(id)
      case JCall(id, name, parent, declId, this0 :: args, _, _) =>
        Access(convert(this0), Identifier(name).setId(id), args map convert).setId(id)
    }
  }

  def convert(jCondition: JCondition): Expression = {
    jCondition match {
      case JWhere(id, condition, _) =>
        convert(condition)
    }
  }

  def convert(jRecordKey: JRecordKey): Parameter = {
    Parameter(jRecordKey.name, makeTypeName(jRecordKey.`type`).setId(jRecordKey.id)).setId(jRecordKey.id)
  }

  def convert(jRecordField: JRecordField): Parameter = {
    Parameter(jRecordField.name, makeTypeName(jRecordField.`type`).setId(jRecordField.id)).setId(jRecordField.id)
  }

  def convert(jLibAction: JLibAction): ActionUsage = {
    ActionUsage(jLibAction.name, jLibAction.inParameters map convert, jLibAction.outParameters map convert).setId(jLibAction.id)
  }

  def convert(jActionBinding: JActionBinding): ActionResolution = {
    ActionResolution(jActionBinding.name, jActionBinding.actionId).setId(jActionBinding.id)
  }

  def convert(jTypeResolution: JTypeBinding): TypeResolution = {
    TypeResolution(jTypeResolution.name, makeTypeName(jTypeResolution.`type`).setId(jTypeResolution.id)).setId(jTypeResolution.id)
  }

  def convert(jInlineAction: JInlineAction): InlineAction = {
    InlineAction(jInlineAction.reference.name,
      jInlineAction.inParameters map convert,
      jInlineAction.outParameters map convert,
      convert(jInlineAction.body),
      makeTypeName(jInlineAction.reference.`type`)).setId(jInlineAction.id)
  }

  def convert(jOptionalParameter: JOptionalParameter): OptionalParameter = {
    OptionalParameter(jOptionalParameter.name, convert(jOptionalParameter.expr)).setId(jOptionalParameter.id)
  }

  def makeTypeName(value: JValue, isSingleton: Boolean): TypeName = {
    value match {

      case JString(x) => TypeName(x, isSingleton = isSingleton)
      case _ =>

        value \ "o" match {
          case JString(x) => return TypeName(x, isSingleton = isSingleton, isUserDefined = true)
          case _ => ()
        }

        value \ "g" match {
          case JString(x) =>
            value \ "a" match {
              case JArray(y) =>
                return TypeName(x, y.map(makeTypeName(_, isSingleton)))
              case _ =>
                return TypeName(x)
            }
          case _ => ()
        }

        throw TouchException("conversion of typename failed: " + value)
    }
  }

  def makeTypeName(typeString: String, isSingleton: Boolean = false): TypeName = {
    if (typeString.startsWith("{")) makeTypeName(parse(typeString), isSingleton = isSingleton)
    else TypeName(typeString, isSingleton = isSingleton)
  }

}

/** When reading the class name from the json type hint field, convert first char to lower case, remove leading J */
case class WebAstTypeHints(hints: List[Class[_]]) extends TypeHints {
  def classFor(hint: String) = hints find (hintFor(_) == hint)

  def hintFor(msgClass: Class[_]): String = {
    val shortNameIdx = msgClass.getName.lastIndexOf(".") + 2
    msgClass.getName.substring(shortNameIdx, shortNameIdx + 1).toLowerCase +
      msgClass.getName.substring(shortNameIdx + 1)
  }
}

object WebAST {

  type JTypeRef = String
  type JNodeRef = String

  @Salat
  trait JNode {
    val id: String
  }

  @Salat
  trait JDecl extends JNode {
    val name: String
  }

  @Salat
  trait JToken extends JNode

  @Salat
  trait JExpr extends JToken

  @Salat
  trait JStmt extends JNode {

    // this is available when using the short form
    val locals: Option[List[JLocalDef]] = None

  }

  @Salat
  trait JCondition extends JNode {
    // this is available when using the short form
    val locals: Option[List[JLocalDef]]
  }

  @Salat
  trait JAbstractInlineParameters

  @Salat
  trait JActionBase extends JDecl {
    val inParameters: List[JLocalDef]
    val outParameters: List[JLocalDef]
    // note that events should be always treated as private, but for historical reasons this field can be true or false
    val isPrivate: Boolean
    val isOffline: Boolean
    val isQuery: Boolean
    val isTest: Boolean
    val isAsync: Boolean
    val description: String
  }

  @Salat
  trait JGlobalDef extends JDecl {
    val comment: String
    val `type`: JTypeRef
    val isReadonly: Boolean
    val isTransient: Boolean
    val isCloudEnabled: Boolean
  }

  @Salat
  trait JBinding extends JNode {
    val id: String
    val name: String
    // name of the formal argument
    val isExplicit: Boolean // was it explicitly specified by the user
    // implicit bindings are ignored when building expressions
  }

  case class JOperator(id: String, op: String) extends JToken

  case class JPropertyRef(
      id: String,
      name: String,
      parent: JTypeRef,
      // if used as token this is ignored when building
      // if used as JCall it's needed for operators
      declId: Option[JNodeRef] // filled when the property is user-defined
  ) extends JToken

  case class JStringLiteral(id: String, value: String) extends JExpr

  case class JBooleanLiteral(id: String, value: Boolean) extends JExpr

  case class JNumberLiteral(id: String, value: Double) extends JExpr

  // when building expressions of these three types you can provide localId/`type` or name,
  // if you provide both, name is ignored
  case class JLocalRef(
      id: String,
      name: String,
      localId: JNodeRef
  ) extends JExpr

  case class JPlaceholder(
      id: String,
      name: String,
      `type`: JTypeRef
  ) extends JExpr

  // A singleton (probably) references one of the top-level categories such as
  // libraries or data. When trying to call "♻ l →  foo(x1, x2)", one may
  // understand that the following call takes place:
  //   ♻ -> l -> foo(x1, x2)
  // and the following AST is generated:
  //   JCall { name: foo, parent: l, args: [
  //     JCall { name: l, parent: ♻, args: [ JSingletonRef ♻ ] },
  //     x1,
  //     x2
  //  ]}
  // this is surprising, because when calling "1 + 2", we generate a call that
  // has two arguments only.
  case class JSingletonRef(
      id: String,
      name: String,
      `type`: JTypeRef
  ) extends JExpr

  case class JCall(
      id: String,
      name: String,
      parent: JTypeRef,
      declId: Option[JNodeRef], // filled when the property is user-defined
      args: List[JExpr],
      // If we are calling a *`type`* T on an expression (e.g. create ->
      // Collection of -> T), then T will be in there.
      typeArgs: Option[List[JTypeRef]] = None,
      // The field below, if present, determines without ambiguity the nature
      // of the call.
      // - extension (the new special syntax)
      // - field (reading a record field)
      // Other types of calls can be determined by careful inspection of the
      // receiver. See the C++ code emitter.
      callType: Option[String] = None
  ) extends JExpr

  // Expressions can be represented in two different manners.
  // - The first one is as a series of tokens. This would correspond to the
  //   "hybrid AST" described in the OOPSLA'15 submission. In that
  //   representation, the [tree] field is null and the [tokens] field
  //   contains the list of tokens.
  // - The second one is as an actual AST, with a proper tree structure. In
  //   that case, the [tokens] field is null and [tree] must contain a proper
  //   tree.
  //
  // TouchDevelop conflates variable binding and expressions. This means that
  // every expression is flagged with the variables that are introduced at
  // this stage. For instance, "var x = 1" will be translated as a
  // [JExprHolder] where [locals] contains a [JLocalDef x], and either:
  // - [tokens] is [JLocalRef x, JOperator :=, JOperator 1], or
  // - [tree] is [JCall { name: ":=", parent: "Unknown", args: [JLocalRef x, JNumberLiteral 1] }]
  //
  // This is not the traditional notion of binding! The variable's scope is
  // not limited to the tokens, but rather extends until the end of the parent
  // block.
  case class JExprHolder(
      id: String,
      // if tokens is unset, will try to use tree
      tokens: Option[List[JToken]],
      tree: JExpr,
      locals: List[JLocalDef] // locals variables defined in this expression
  ) extends JNode

  case class JComment(id: String, text: String) extends JStmt

  case class JFor(id: String,
      index: JLocalDef,
      bound: JExprHolder,
      body: List[JStmt]
  ) extends JStmt

  case class JForeach(
      id: String,
      iterator: JLocalDef,
      collection: JExprHolder,
      conditions: List[JCondition],
      body: List[JStmt]
  ) extends JStmt

  case class JWhere(id: String, condition: JExprHolder, locals: Option[List[JLocalDef]]) extends JCondition

  case class JWhile(
      id: String,
      condition: JExprHolder,
      body: List[JStmt]
  ) extends JStmt

  // Sequences of if / else if / else statements are not represented the usual
  // way. That is, instead of having a structured AST:
  //
  // if
  // |- condition1
  // |- then-branch1 = ...
  // |- else-branch = if
  //                  |- condition2
  //                  |- then-branch2
  //                  |- else-branch2
  //
  // the TouchDevelop AST adopts the following (unusual) representation.
  //
  // if
  // |- condition1
  // |- then-branch1 = ...
  // |- else-branch = null
  // if
  // |- condition2
  // |- then-branch2
  // |- else-branch2
  // |- isElseIf = true
  //
  // This is NOT equivalent to the representation above (condition2 may
  // subsume condition1), so the extra flag "isElseIf" is set and (I suppose)
  // gets some special treatment when it comes to running / compiling the
  // program.

  case class JContinue(id: String) extends JExpr

  case class JBreak(id: String) extends JExpr

  case class JReturn(id: String, expr: JExpr) extends JExpr

  case class JShow(id: String, expr: JExpr) extends JExpr

  case class JIf(
      id: String,
      condition: JExprHolder,
      thenBody: List[JStmt],
      elseBody: List[JStmt],
      isElseIf: Boolean
  ) extends JStmt

  case class JBoxed(id: String, body: List[JStmt]) extends JStmt

  case class JExprStmt(id: String, expr: JExprHolder) extends JStmt

  case class JInlineActions(id: String, expr: JExprHolder, actions: List[JAbstractInlineParameters]) extends JStmt

  case class JInlineAction(
      id: String,
      reference: JLocalDef,
      inParameters: List[JLocalDef],
      outParameters: List[JLocalDef],
      body: List[JStmt],
      locals: Option[List[JLocalDef]], // this contains the reference in short mode, it never contains anything else
      isImplicit: Option[Boolean],
      isOptional: Option[Boolean]
  ) extends JNode with JAbstractInlineParameters

  case class JOptionalParameter(
      id: String,
      name: String,
      declId: JNodeRef,
      expr: JExprHolder
  ) extends JNode with JAbstractInlineParameters

  case class JActionType(
      id: String,
      name: String,
      inParameters: List[JLocalDef],
      outParameters: List[JLocalDef],
      // note that events should be always treated as private, but for historical reasons this field can be true or false
      isPrivate: Boolean,
      isOffline: Boolean,
      isQuery: Boolean,
      isTest: Boolean,
      isAsync: Boolean,
      description: String) extends JActionBase

  case class JAction(
      id: String,
      name: String,
      inParameters: List[JLocalDef],
      outParameters: List[JLocalDef],
      // note that events should be always treated as private, but for historical reasons this field can be true or false
      isPrivate: Boolean,
      isOffline: Boolean,
      isQuery: Boolean,
      isTest: Boolean,
      isAsync: Boolean,
      description: String,
      body: List[JStmt]) extends JActionBase

  case class JPage(
      id: String,
      name: String,
      inParameters: List[JLocalDef],
      outParameters: List[JLocalDef],
      // note that events should be always treated as private, but for historical reasons this field can be true or false
      isPrivate: Boolean,
      isOffline: Boolean,
      isQuery: Boolean,
      isTest: Boolean,
      isAsync: Boolean,
      description: String,
      initBody: List[JStmt],
      displayBody: List[JStmt],
      initBodyId: Option[String],
      displayBodyId: Option[String],
      hasModelParameter: Option[Boolean]
  ) extends JDecl

  case class JEvent(
      id: String,
      name: String,
      inParameters: List[JLocalDef],
      outParameters: List[JLocalDef],
      // note that events should be always treated as private, but for historical reasons this field can be true or false
      isPrivate: Boolean,
      isOffline: Boolean,
      isQuery: Boolean,
      isTest: Boolean,
      isAsync: Boolean,
      description: String,
      // when building provide name or both eventName and eventVariableId (which take precedence over name)
      eventName: Option[String],
      eventVariableId: Option[JNodeRef],
      body: List[JStmt]
  ) extends JActionBase

  case class JLibAction(
      id: String,
      name: String,
      inParameters: List[JLocalDef],
      outParameters: List[JLocalDef],
      // note that events should be always treated as private, but for historical reasons this field can be true or false
      isPrivate: Boolean,
      isOffline: Boolean,
      isQuery: Boolean,
      isTest: Boolean,
      isAsync: Boolean,
      description: String,
      parentLibId: JNodeRef // this can be empty - it means "current script"
  ) extends JActionBase

  case class JLibAbstractType(
      id: String,
      name: String
  ) extends JDecl

  case class JLibActionType(
      id: String,
      name: String,
      inParameters: List[JLocalDef],
      outParameters: List[JLocalDef],
      // note that events should be always treated as private, but for historical reasons this field can be true or false
      isPrivate: Boolean,
      isOffline: Boolean,
      isQuery: Boolean,
      isTest: Boolean,
      isAsync: Boolean,
      description: String
  ) extends JActionBase

  case class JLibRecordType(
      id: String,
      name: String,
      sourceName: Option[String],
      comment: String,
      category: String, // "object", "table", "index", or "decorator"
      isCloudEnabled: Boolean,
      isCloudPartiallyEnabled: Boolean,
      isPersistent: Boolean,
      isExported: Boolean,
      keys: List[JRecordKey],
      fields: List[JRecordField]
  ) extends JDecl

  case class JArt(
      id: String,
      name: String,
      comment: String,
      `type`: JTypeRef,
      isReadonly: Boolean,
      isTransient: Boolean,
      isCloudEnabled: Boolean,
      url: String,
      // If it's a String art, contains its value.
      value: Option[String]
  ) extends JGlobalDef

  case class JData(
      id: String,
      name: String,
      comment: String,
      `type`: JTypeRef,
      isReadonly: Boolean,
      isCloudEnabled: Boolean,
      isTransient: Boolean
  ) extends JGlobalDef

  case class JLibrary(
      id: String,
      name: String,
      libIdentifier: String,
      libIsPublished: Boolean,
      scriptName: String, // name of the script to which the library resolves
      exportedTypes: String, // space separated, obsolete, use exportedTypeDefs
      exportedTypeDefs: List[JDecl], // JLibAbstractType or JLibActionType
      exportedActions: List[JLibAction],
      resolveClauses: List[JResolveClause]
  ) extends JDecl

  case class JTypeBinding(
      id: String,
      name: String,
      isExplicit: Boolean, // was it explicitly specified by the user
      `type`: JTypeRef
  ) extends JBinding

  case class JActionBinding(
      id: String,
      name: String,
      isExplicit: Boolean, // was it explicitly specified by the user
      actionId: JNodeRef
  ) extends JBinding

  case class JResolveClause(
      id: String,
      name: String,
      // points to a JLibrary (not publish-id),
      // it may be null for binding to the current script
      defaultLibId: JNodeRef,
      withTypes: List[JTypeBinding],
      withActions: List[JActionBinding]
  ) extends JNode

  case class JRecord(
      id: String,
      name: String,
      sourceName: Option[String],
      comment: String,
      category: String, // "object", "table", "index", or "decorator"
      isCloudEnabled: Boolean,
      isCloudPartiallyEnabled: Boolean,
      isPersistent: Boolean,
      isExported: Boolean,
      keys: List[JRecordKey],
      fields: List[JRecordField]
  ) extends JDecl


  case class JRecordField(
      id: String,
      name: String,
      `type`: JTypeRef
  ) extends JNode

  case class JRecordKey(
      id: String,
      name: String,
      `type`: JTypeRef
  ) extends JNode

  // local variable or a parameter
  case class JLocalDef(
      id: String,
      name: String,
      `type`: JTypeRef
  ) extends JNode

  // Response to:
  // GET /api/<script-id>/webast
  case class JApp(
      id: String,
      // both versions are comma-separated list of tokens/features
      textVersion: String,
      jsonVersion: String,

      name: String,
      comment: String,
      // The name and icon are only given here if they are explicitly specified by the user.
      icon: Option[String], // name of the icon, e.g., "Bolt"
      color: Option[String], // e.g., #ff00ff
      // These two are always present. They are ignored when building new scripts.
      autoIcon: String,
      autoColor: String,

      platform: String, // comma-separated
      isLibrary: Boolean,
      showAd: Boolean,
      hasIds: Boolean, // does it have stable, persistent ids for every stmt
      rootId: String,
      decls: List[JDecl],
      deletedDecls: List[JDecl], // these are present when a node was deleted but is still referenced from somewhere

      libraryName: Option[String], // when used in reflection info
      libraryId: Option[String] // when used in reflection info
  ) extends JNode


  //
  // API description
  //

  case class JPropertyParameter(
      name: String,
      `type`: JTypeRef,
      writesMutable: Option[Boolean], // are fields of the object referenced by this paramter being written to
      readsMutable: Option[Boolean], // .... read from
      defaultValue: Option[List[JToken]],
      stringValues: Option[List[String]] // these show up in intelli buttons, they are usually all allowed values for a parameter
  )

  case class JProperty(
      name: String,
      help: String,
      usage_count: Int, // this is used for syntax autocompletion priority
      runOnInvalid: Option[Boolean], // should the property by run even if one of the arguments is 'invalid'
      isHidden: Option[Boolean], // doesn't show in autocompletion
      isAsync: Option[Boolean],
      isObsolete: Option[Boolean], // generates a warning
      isDbgOnly: Option[Boolean], // an experimental feature, not visible in regular builds
      isBetaOnly: Option[Boolean], // a feature in testing, visible in /app/beta
      jsName: String, // how is the property refered to from JavaScript
      infixPriority: Option[Int], // when present, this is an infix operator with given priority
      // higher Int is higher priority, even assosiates left, odd - right
      pausesInterpreter: Option[Boolean], // is this a potentially-async operation
      usesStackFrame: Option[Boolean], // is the implementation passed IStackFrame object
      missingWeb: Option[Boolean], // is the implementation missing from the general web version
      missingWab: Option[Boolean], // .... from web version running with WebAppBooster
      capabilities: Option[String], // comma-separated list of required platform capabilities (if any)
      result: JPropertyParameter,
      parameters: List[JPropertyParameter]
  )

  case class JTypeDef(
      name: String,
      help: String,
      icon: String, // a name of the icon representing this `type`
      isAction: Option[Boolean], // is it a function `type`, look for 'run' property for the signature
      isData: Boolean, // false for singleton types
      stemName: String, // used when auto-naming variables of this `type`
      jsName: String, // how is the `type` refered to from JavaScript
      isDbgOnly: Option[Boolean], // an experimental feature, not visible in regular builds
      isBetaOnly: Option[Boolean], // a feature in testing, visible in /app/beta
      isSerializable: Boolean, // do we support automatic serialization of this `type`
      isBuiltin: Option[Boolean], // true for Int, Boolean, String, the JS calling convention is different for these
      ctxLocal: Option[Boolean], // can it be used as local variable
      ctxGlobal: Option[Boolean], // .... as global variable
      ctxField: Option[Boolean], // .... as field of a record
      ctxLocalKey: Option[Boolean], // .... as key in a local index
      ctxGcKey: Option[Boolean], // can it have decorators
      ctxCloudKey: Option[Boolean],
      ctxRowKey: Option[Boolean],
      ctxCloudField: Option[Boolean],
      ctxWallTap: Option[Boolean], // do global variables of this `type` get 'wall tap' events
      ctxEnumerable: Option[Boolean], // can it be used with foreach construct
      ctxJson: Option[Boolean], // can it be json exported/imported
      properties: List[JProperty]
  )

  // GET /api/language/apis
  case class JApis(
      textVersion: String,
      jsonVersion: String,
      types: List[JTypeDef]
  )

}

/*

The short format
~~~~~~~~~~~~~~~~

The main difference between the full JSON format and the short JSON format is
representation of `JExprHolder` nodes. Whenever the full JSON format says node
`JBar` has a field `foo` of `type` `JExprHolder`, then in the short format `JBar`
has a field `foo` of `type` `String` and a field `locals` of `type` `List[JLocalDef]`.
Additionally, the fields `index` in `JFor` and `iterator` in `JForeach` are
absent, and the loop-bound variable is instead stored as the first element of
`locals`.

The String placed instead of the `JExprHolder` node can be turned into sequence
of tokens using the following function:

    export function shortToTokens(shortForm:String)
    {
        function uq(s:String) {
            var r = ""
            for (var i = 0, i < s.length, ++i) {
                var c = s.charAt(i),
                if (c == "_") {
                    r += " ",
                } else if (c == "/") {
                    r += String.fromCharCode(parseInt(s.slice(i + 1, i + 5), 16))
                    i += 4
                } else {
                    r += c,
                }
            }
            return r,
        }

        function oneToken(s:String) {
            var v = s.slice(1)
            switch (s[0]) {
                case ",": return { nodeType: "operator", op: v }
                case "#": return { nodeType: "propertyRef", declId: v }
                case ".": return { nodeType: "propertyRef", name: uq(v) }
                case "'": return { nodeType: "stringLiteral", value: uq(v) }
                case "F":
                case "T": return { nodeType: "booleanLiteral", value: (s[0] == "T") }
                case "$": return { nodeType: "localRef", localId: v }
                case ":": return { nodeType: "singletonRef", name: uq(v) }
                case "?":
                    var cln = v.indexOf(':')
                    if (cln > 0)
                        return { nodeType: "placeholder", `type`: uq(v.slice(0, cln)), name: uq(v.slice(cln + 1)) }
                    else
                        return { nodeType: "placeholder", `type`: uq(v) }
                default:
                    throw new Error("wrong short form: " + s)
            }
        }

        if (!shortForm) return [], // handles "" and null, the code below is incorrect for ""

        return shortForm.split(" ").map(oneToken)
    }

In other words, it's space separated sequence of strings, where the first
character denotes the kind of token and remaining characters are the payload.
The String is quoted by replacing spaces with underscores and all other
non-alphanumeric characters with unicode sequences preceeded by a slash (not
backslash to avoid double quoting in JSON).



Short diff format
~~~~~~~~~~~~~~~~~

Every object node in the short JSON format has a field named `id`. This is used
when formulating diffs. The diff is set of updates to nodes of given ids. For
every id there is a set of `fieldName`, `value` pairs.

For example consider:

A = {
  "id": "01",
  "one": "one",
  "two": 2,
  "baz": [
    { "id": "02", "enabled": true },
    { "id": "03", "x": 0 }
  ]
}

B = {
  "id": "01",
  "one": "seven",
  "two": 2,
  "baz": [
    { "id": "02", "enabled": true },
    { "id": "05", "y": 7, "z": 13 }
  ]
}

diff(A, B) = {
  // new node, assignment given for all fields
  "05": { "y": 7, "z": 13 },
  // updated node
  "01": {
    "one": "seven", // the field "one" is now "seven"
    // the field "two" is not mentioned and thus unchanged
    // the field "baz" now contains two nodes, ids of which are given
    "baz": [ "02", "05" ]
  },
  // the node "03" is deleted
  "03": null
}

The JSON diff relies on the following properties of the short JSON format:

Fields of JNodes always contain either:
  1. a JSON primitive value (String, Boolean, integer, null), or
  2. a sequence of JNodes

Every JNode has a unique 'id' field.

This is why JFor.bound and JForeach.collection fields are missing.  In the diff
format sequence of strings is always treated as a sequence of node ids.

The following function can be used to apply JSON diff:

    function indexIds(obj:any)
    {
        var oldById:any = {}

        function findIds(o:any) {
            if (!o) return,
            if (List.isList(o)) {
                for (var i = 0, i < o.length, ++i)
                    findIds(o[i])
            } else if (typeof o === "object") {
                if (!o.id) Util.oops("no id for " + JSON.stringify(o))
                if (oldById.hasOwnProperty(o.id)) Util.oops("duplicate id " + o.id)
                oldById[o.id] = o
                var k = Object.keys(o)
                for (var i = 0, i < k.length, ++i)
                    findIds(o[k[i]])
            }
        }
        findIds(obj)

        return oldById
    }

    export function applyJsonDiff(base:any, diff:any)
    {
        var byId = indexIds(base)

        var k = Object.keys(diff)
        for (var i = 0, i < k.length, ++i) {
            var id = k[i]
            var upd = diff[id]
            if (upd === undefined) continue,
            var trg = byId[id]
            if (upd === null) {
                if (!trg) Util.oops("apply diff: no target id " + id)
                trg.__deleted = true,
                continue,
            }
            if (!trg) {
                byId[id] = trg = { id: id }
            }
            var kk = Object.keys(upd)
            for (var j = 0, j < kk.length, ++j) {
                var f = kk[j]
                var v = upd[f]
                if (List.isList(v) && typeof v[0] === "String")
                    v = v.map(id => {
                        var r = byId[id]
                        if (!r) { r = byId[id] = { id: id } }
                        return r
                    })

                Util.assert(f != "nodeType" || !trg[f])
                trg[f] = v
            }
        }

        var newIds = indexIds(base)
        k = Object.keys(newIds)
        for (var i = 0, i < k.length, ++i) {
            var id = k[i]
            if (newIds[k[i]].__deleted)
                Util.oops("dangling id after diff " + id)
        }
    }

*/
