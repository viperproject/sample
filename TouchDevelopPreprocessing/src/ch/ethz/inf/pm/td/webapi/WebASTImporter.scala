package ch.ethz.inf.pm.td.webapi

import net.liftweb.json.{TypeHints, DefaultFormats}
import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.parse
import ch.ethz.inf.pm.td.parser._

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
      classOf[JIf], classOf[JBoxed], classOf[JExprStmt], classOf[JInlineActions], classOf[JInlineAction], classOf[JAction],
      classOf[JPage], classOf[JEvent], classOf[JLibAction], classOf[JArt], classOf[JData], classOf[JLibrary],
      classOf[JTypeBinding], classOf[JActionBinding], classOf[JResolveClause], classOf[JRecord], classOf[JRecordField],
      classOf[JRecordKey], classOf[JLocalDef], classOf[JApp], classOf[JPropertyParameter], classOf[JProperty],
      classOf[JTypeDef], classOf[JApis], classOf[JCall])
    )
  }

  def queryAndConvert(pubID: String): Script = {
    convert(query(pubID))
  }

  def query(pubID: String): JApp = {
    val url = ScriptListings.webastURLfromPubID(pubID)
    val string = URLFetcher.fetchFile(url)
    val json = parse(string)
    json.asInstanceOf[JObject].extract[JApp]
  }

  def convertFromString(string: String): Script = {
    val json = parse(string)
    val japp = json.asInstanceOf[JObject].extract[JApp]
    convert(japp)
  }

  def convert(jAST:JApp):Script = {
    Script(jAST.decls map (convert _),jAST.isLibrary).setId("")
  }

  def convert(jDecl:JDecl):Declaration = {
    jDecl match {
      case JArt(id,name,comment,typ,isReadonly,url) =>
        VariableDefinition(Parameter(name,TypeName(typ).setId(id)).setId(id),Map("readonly" -> isReadonly.toString,"is_resource" -> "true")).setId(id)
      case JData(id,name,comment,typ,isReadonly) =>
        VariableDefinition(Parameter(name,TypeName(typ).setId(id)).setId(id),Map("readonly" -> isReadonly.toString)).setId(id)
      case JPage(id,name,inParameters,outParameters,isPrivate,isOffloaded,isTest,initBody,displayBody) =>
        PageDefinition(name,inParameters map (convert _),outParameters map (convert _),initBody map (convert _),displayBody map (convert _),isPrivate).setId(id)
      case JEvent(id,name,inParameters,outParameters,isPrivate,isOffloaded,isTest,eventName,eventVariableId,body) =>
        ActionDefinition(name,inParameters map (convert _),outParameters map (convert _),body map (convert _),isEvent = true,isPrivate = isPrivate).setId(id)
      case JLibrary(id,name,libIdentifier,libIsPublished,exportedTypes,exportedActions,resolveClauses) =>
        LibraryDefinition(name,libIdentifier,exportedActions map (convert _),resolveClauses map (convert _)).setId(id)
      case JRecord(id,name,comment,category,isCloudEnabled,keys,fields) =>
        TableDefinition(name,category,keys map (convert _),fields map (convert _)).setId(id)
      case JAction(id,name,inParameters,outParameters,isPrivate,isOffloaded,isTest,body) =>
        ActionDefinition(name,inParameters map (convert _),outParameters map (convert _),body map (convert _),isEvent = false,isPrivate = isPrivate).setId(id)
    }
  }

  def convert(jResolve:JResolveClause):ResolveBlock = {
    ResolveBlock(jResolve.name,jResolve.defaultLibId,
      (jResolve.withActions map (convert _)) ::: (jResolve.withTypes map (convert _))).setId(jResolve.id)
  }

  def convert(jLocalDef:JLocalDef):Parameter = {
    Parameter(jLocalDef.name,TypeName(jLocalDef.`type`).setId(jLocalDef.id)).setId(jLocalDef.id)
  }

  def convert(jStatement:JStmt):Statement = {
    jStatement match {
      case JComment(id,text) =>
        Skip().setId(id)
      case JFor(id,index,bound,body) =>
        For(index.name,convert(bound),body map (convert _)).setId(id)
      case JForeach(id,iterator,collection,conditions,body) =>
        Foreach(iterator.name,convert(collection),conditions map (convert _),body map (convert _)).setId(id)
      case JWhile(id,condition,body) =>
        While(convert(condition),body map (convert _)).setId(id)
      case JIf(id,condition,thenBody,elseBody) =>
        If(convert(condition),thenBody map (convert _), elseBody map (convert _)).setId(id)
      case JBoxed(id,body) =>
        Box(body map (convert _)).setId(id)
      case JExprStmt(id,expr) =>
        ExpressionStatement(convert(expr)).setId(id)
      case JInlineActions(id,expr,actions) =>
        WhereStatement(convert(expr),actions map (convert _)).setId(id)
    }
  }

  def convert(jExpression:JExprHolder):Expression = {
    convert(jExpression.tree)
  }

  def convert(jExpression:JExpr):Expression = {
    jExpression match {
      case JStringLiteral(id,value) =>
        Literal(TypeName("String").setId(id),value).setId(id)
      case JBooleanLiteral(id,value) =>
        Literal(TypeName("Boolean").setId(id),value.toString).setId(id)
      case JNumberLiteral(id,value) =>
        Literal(TypeName("Number").setId(id),value.toString).setId(id)
      case JLocalRef(id,name,localId) =>
        LocalReference(name).setId(id)
      case JPlaceholder(id,name,typ) =>
        LocalReference(name).setId(id)
      case JSingletonRef(id,name,typ) =>
        SingletonReference(name,typ).setId(id)
      case JCall(id,name,parent,declId,this0::args) =>
        Access(convert(this0),Identifier(name).setId(id),args map (convert _)).setId(id)
    }
  }

  def convert(jCondition:JCondition):Expression = {
    jCondition match {
      case JWhere(id,condition) =>
        convert(condition)
    }
  }

  def convert(jRecordKey:JRecordKey):Parameter = {
    Parameter(jRecordKey.name,TypeName(jRecordKey.`type`).setId(jRecordKey.id)).setId(jRecordKey.id)
  }

  def convert(jRecordField:JRecordField):Parameter = {
    Parameter(jRecordField.name,TypeName(jRecordField.`type`).setId(jRecordField.id)).setId(jRecordField.id)
  }

  def convert(jLibAction:JLibAction):ActionUsage = {
    ActionUsage(jLibAction.name,jLibAction.inParameters map (convert _),jLibAction.outParameters map (convert _)).setId(jLibAction.id)
  }

  def convert(jActionBinding:JActionBinding):ActionResolution = {
    ActionResolution(jActionBinding.name,jActionBinding.actionId).setId(jActionBinding.id)
  }

  def convert(jTypeResolution:JTypeBinding):TypeResolution = {
    TypeResolution(jTypeResolution.name,TypeName(jTypeResolution.`type`).setId(jTypeResolution.id)).setId(jTypeResolution.id)
  }

  def convert(jInlineAction:JInlineAction):InlineAction = {
    InlineAction(jInlineAction.reference.name,
      jInlineAction.inParameters map (convert _),
      jInlineAction.outParameters map (convert _),
      jInlineAction.body map (convert _)).setId(jInlineAction.id)
  }

}

/** When reading the class name from the json type hint field, convert first char to lower case, remove leading J */
case class WebAstTypeHints(hints: List[Class[_]]) extends TypeHints {
  def hintFor(msgClass: Class[_]): String = {
    val shortNameIdx = msgClass.getName.lastIndexOf(".") + 2
    msgClass.getName.substring(shortNameIdx, shortNameIdx + 1).toLowerCase +
      msgClass.getName.substring(shortNameIdx + 1)
  }

  def classFor(hint: String) = hints find (hintFor(_) == hint)
}

class JNode(id: String)

class JDecl(id: String, name: String) extends JNode(id)

class JToken(id: String) extends JNode(id)

class JExpr(id: String) extends JToken(id)

case class JOperator(id: String, op: String) extends JToken(id)

case class JPropertyRef(
                         id: String,
                         name: String,
                         parent: String /*JTypeRef*/ ,
                         declId: Option[String /*JNodeRef*/ ] // filled when the property is user-defined
                         ) extends JToken(id)

case class JStringLiteral(id: String, value: String) extends JExpr(id)

case class JBooleanLiteral(id: String, value: Boolean) extends JExpr(id)

case class JNumberLiteral(id: String, value: Double) extends JExpr(id)

// when building expressions of these three types you can provide localId/`type` or name,
// if you provide both, name is ignored
case class JLocalRef(
                      id: String,
                      name: String,
                      localId: String /*JNodeRef*/
                      ) extends JExpr(id)

case class JPlaceholder(
                         id: String,
                         name: String,
                         `type`: String /*JTypeRef*/
                         ) extends JExpr(id)

case class JSingletonRef(
                          id: String,
                          name: String,
                          `type`: String /*JTypeRef*/
                          ) extends JExpr(id)

case class JCall(
                  id: String,
                  name: String,
                  parent: String /*JTypeRef*/ ,
                  declId: Option[String /*JNodeRef*/ ], // filled when the property is user-defined
                  args: List[JExpr]
                  ) extends JExpr(id)

case class JExprHolder(
                        id: String,
                        // when building, provide tokens or tree, if you provide both tokens is ignored
                        tokens: List[JToken],
                        tree: JExpr,
                        locals: List[JLocalDef] // locals variables defined in this expression
                        ) extends JNode(id)

class JStmt(id: String) extends JNode(id)

case class JComment(id: String, text: String) extends JStmt(id)

case class JFor(id: String,
                index: JLocalDef,
                bound: JExprHolder,
                body: List[JStmt]
                 ) extends JStmt(id)

case class JForeach(
                     id: String,
                     iterator: JLocalDef,
                     collection: JExprHolder,
                     conditions: List[JCondition],
                     body: List[JStmt]
                     ) extends JStmt(id)

class JCondition(id: String) extends JNode(id)

case class JWhere(id: String, condition: JExprHolder) extends JCondition(id)

case class JWhile(
                   id: String,
                   condition: JExprHolder,
                   body: List[JStmt]
                   ) extends JStmt(id)

case class JIf(
                id: String,
                condition: JExprHolder,
                thenBody: List[JStmt],
                elseBody: List[JStmt]
                ) extends JStmt(id)

case class JBoxed(id: String, body: List[JStmt]) extends JStmt(id)

case class JExprStmt(id: String, expr: JExprHolder) extends JStmt(id)

case class JInlineActions(id: String, expr: JExprHolder, actions: List[JInlineAction]) extends JStmt(id)

case class JInlineAction(
                          id: String,
                          reference: JLocalDef,
                          inParameters: List[JLocalDef],
                          outParameters: List[JLocalDef],
                          body: List[JStmt]
                          ) extends JNode(id)

case class JAction(
                    id: String,
                    name: String,
                    inParameters: List[JLocalDef],
                    outParameters: List[JLocalDef],
                    // note that events should be always treated as private, but for historical reasons this field can be true or false
                    isPrivate: Boolean,
                    isOffloaded: Boolean,
                    isTest: Boolean,
                    body: List[JStmt]) extends JDecl(id, name)

case class JPage(
                  id: String,
                  name: String,
                  inParameters: List[JLocalDef],
                  outParameters: List[JLocalDef],
                  // note that events should be always treated as private, but for historical reasons this field can be true or false
                  isPrivate: Boolean,
                  isOffloaded: Boolean,
                  isTest: Boolean,
                  initBody: List[JStmt],
                  displayBody: List[JStmt]
                  ) extends JDecl(id, name)

case class JEvent(
                   id: String,
                   name: String,
                   inParameters: List[JLocalDef],
                   outParameters: List[JLocalDef],
                   // note that events should be always treated as private, but for historical reasons this field can be true or false
                   isPrivate: Boolean,
                   isOffloaded: Boolean,
                   isTest: Boolean,
                   // when building provide name or both eventName and eventVariableId (which take precedence over name)
                   eventName: String,
                   eventVariableId: String /*JNodeRef*/ ,
                   body: List[JStmt]
                   ) extends JDecl(id, name)

case class JLibAction(
                       id: String,
                       name: String,
                       inParameters: List[JLocalDef],
                       outParameters: List[JLocalDef],
                       // note that events should be always treated as private, but for historical reasons this field can be true or false
                       isPrivate: Boolean,
                       isOffloaded: Boolean,
                       isTest: Boolean,
                       parentLibId: String /*JNodeRef*/
                       // this can be empty - it means "current script"
                       )


class JGlobalDef(
                  id: String,
                  name: String,
                  comment: String,
                  `type`: String /*JTypeRef*/ ,
                  isReadonly: Boolean
                  ) extends JDecl(id, name)

case class JArt(
                 id: String,
                 name: String,
                 comment: String,
                 `type`: String /*JTypeRef*/ ,
                 isReadonly: Boolean,
                 url: String) extends JGlobalDef(id, name, comment, `type`, isReadonly)

case class JData(
                  id: String,
                  name: String,
                  comment: String,
                  `type`: String /*JTypeRef*/ ,
                  isReadonly: Boolean
                  ) extends JGlobalDef(id, name, comment, `type`, isReadonly)

case class JLibrary(
                     id: String,
                     name: String,
                     libIdentifier: String,
                     libIsPublished: Boolean,
                     exportedTypes: List[String /*JTypeRef*/ ],
                     exportedActions: List[JLibAction],
                     resolveClauses: List[JResolveClause]
                     ) extends JDecl(id, name)

class JBinding(
                id: String,
                name: String, // name of the formal argument
                isExplicit: Boolean // was it explicitly specified by the user
                // implicit bindings are ignored when building expressions
                ) extends JNode(id)

case class JTypeBinding(
                         id: String,
                         name: String,
                         isExplicit: Boolean, // was it explicitly specified by the user
                         `type`: String /*JTypeRef*/) extends JBinding(id, name, isExplicit)

case class JActionBinding(
                           id: String,
                           name: String,
                           isExplicit: Boolean, // was it explicitly specified by the user
                           actionId: String /*JNodeRef*/) extends JBinding(id, name, isExplicit)

case class JResolveClause(
                           id: String,
                           name: String,
                           defaultLibId: String /*JNodeRef*/ , // points to a JLibrary (not publish-id)
                           withTypes: List[JTypeBinding],
                           withActions: List[JActionBinding]
                           ) extends JNode(id)

case class JRecord(
                    id: String,
                    name: String,
                    comment: String,
                    category: String, // "object", "table", "index", or "decorator"
                    isCloudEnabled: Boolean,
                    keys: List[JRecordKey],
                    fields: List[JRecordField]
                    ) extends JDecl(id, name)

case class JRecordField(
                         id: String,
                         name: String,
                         `type`: String /*JTypeRef*/
                         ) extends JNode(id)

case class JRecordKey(
                       id: String,
                       name: String,
                       `type`: String /*JTypeRef*/
                       ) extends JNode(id)

// local variable or a parameter
case class JLocalDef(
                      id: String,
                      name: String,
                      `type`: String /*JTypeRef*/
                      ) extends JNode(id)

// Response to:
// GET /api/<script-id>/webast
case class JApp(
                 id: String,
                 // both versions are comma-separated list of tokens/features
                 textVersion: String,
                 jsonVersion: String,
                 name: String,
                 comment: String,
                 icon: Option[String], // name of the icon, e.g., "Bolt"
                 color: Option[String], // e.g., #ff00ff
                 platform: String, // comma-separated
                 isLibrary: Boolean,
                 allowExport: Boolean,
                 showAd: Boolean,
                 decls: List[JDecl]
                 ) extends JNode(id)


//
// API description
//

case class JPropertyParameter(
                               name: String,
                               `type`: String /*JTypeRef*/ ,
                               writesMutable: Option[Boolean], // are fields of the object referenced by this paramter being written to
                               readsMutable: Option[Boolean] // .... read from
                               // We could also expose default values for parameter, let us know if you need that!
                               )

case class JProperty(
                      name: String,
                      help: String,
                      usage_count: Int, // this is used for syntax autocompletion priority
                      runOnInvalid: Option[Boolean], // should the property by run even if one of the arguments is 'invalid'
                      isHidden: Option[Boolean], // doesn't show in autocompletion
                      isObsolete: Option[Boolean], // generates a warning
                      isDbgOnly: Option[Boolean], // an experimental feature, not visible in regular builds
                      jsName: String, // how is the property refered to from JavaScript
                      infixPriority: Option[Int], // when present, this is an infix operator with given priority
                      // higher Int is higher priority, even assosiates left, odd - right
                      pausesInterpreter: Option[Boolean], // is this a potentially-async operation
                      usesStackFrame: Option[Boolean], // is the implementation passed IStackFrame object
                      missingWeb: Option[Boolean], // is the implementation missing from the general web version
                      missingWinRT: Option[Boolean], // .... from the runtime for WinRT apps
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
                     isDbgOnly: Option[Boolean], // an experimental feature, not visible in regular builds
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
                     properties: List[JProperty]
                     )

// GET /api/langauge/apis
case class JApis(
                  textVersion: String,
                  jsonVersion: String,
                  types: List[JTypeDef]
                  )