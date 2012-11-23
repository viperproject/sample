package ch.ethz.inf.pm.td.parser

import util.parsing.combinator._
import org.apache.commons.lang3.StringEscapeUtils
import ch.ethz.inf.pm.td.compiler.TouchException

object ScriptParser extends RegexParsers with PackratParsers {

  /** not only ignore white space but also comments */
  //protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  // Top Level and Meta Information

  lazy val script: PackratParser[Script] = positioned (
    declaration.* ^^ (Script(_))
  )

  lazy val declaration: PackratParser[Declaration] = positioned (
    metaDeclaration | actionDefinition | variableDefinition | libraryImport | tableDefinition
  )

  lazy val metaDeclaration: PackratParser[MetaDeclaration] = positioned (
    "meta" ~> ident ~ stringLiteral <~ ";" ^^ { case a~b => MetaDeclaration(a,b) }
  )

  // Actions, Events, Global Variables

  lazy val actionDefinition: PackratParser[ActionDefinition] = positioned (
    ("action" ~ actionHeader ~ block | "event" ~ actionHeader ~ block) ^^ {
      case "action"~a~b => ActionDefinition(a._1,a._2,a._3,b,false)
      case "event"~a~b => ActionDefinition(a._1,a._2,a._3,b,true)
    }
  )

  lazy val actionHeader: PackratParser[(String,List[Parameter],List[Parameter])] = (
    ident ~ "(" ~ repsep(parameter, ",") ~ ")" ~ ("returns" ~ repsep(parameter, ",")).? ^^
      {case i~_~in~_~ret => ret match {case Some(_~out) => (i,in,out); case None => (i,in,Nil) }}
  )
  
  lazy val parameter: PackratParser[Parameter] = positioned (
    ident ~ ":" ~ typeName ^^ {case a~_~b => Parameter(a,b)}
  )

  lazy val typeName: PackratParser[TypeName] = positioned (
    ident ^^ (TypeName(_))
    | libraryReference ~ "→" ~ ident ^^ {case a~_~b => TypeName(b,Some(a))}
  )

  lazy val variableDefinition: PackratParser[VariableDefinition] = positioned (
    "var" ~ parameter ~ "{" ~ variableFlag.* ~ "}" ^^ {case _~a~_~b~_ => VariableDefinition(a,b toMap)}
  )

  lazy val variableFlag: PackratParser[(String,Any)] = (
    ( ident ~ "=" ~ stringLiteral ~ ";" | ident ~ "=" ~ booleanLiteral ~ ";" ) ^^ { case (a~_~b~_) => (a,b) }
  )

  // Tables

  lazy val tableDefinition: PackratParser[TableDefinition] = positioned (
    "table" ~ ident ~ "{" ~ tableContent ~ "}" ^^ {case _~a~_~b~_ => TableDefinition(a,b._1,b._2,b._3)}
  )

  lazy val tableContent: PackratParser[(String,List[Parameter],List[Parameter])] = (
    ( "type" ~ "=" ~ stringLiteral ~ ";" ~ ("keys" ~ "{" ~ parameter.* ~ "}").? ~ ("fields" ~ "{" ~ parameter.* ~ "}").? )
      ^^ {case _~_~a~_~b~c => (a, b match { case Some(_~_~x~_) => x ; case None => Nil }, c match { case Some(_~_~x~_) => x ; case None => Nil } )}
  )

  // Library Imports

  lazy val libraryImport: PackratParser[Declaration] = positioned (
    "meta" ~ "import" ~ ident ~ "{" ~ ("pub" ~> stringLiteral) ~ libraryUsage ~ resolveBlock.* ~ "}"
      ^^ { case _~_~name~_~pub~use~res~_ => LibraryDefinition(name,pub,use,res) }
  )

  lazy val libraryUsage: PackratParser[List[UsageDeclaration]] =  (
    "usage" ~ "{" ~ typeUsage.* ~ actionUsage.* ~ "}"
      ^^ { case _~_~use~act~_ => use ::: act }
  )

  lazy val typeUsage: PackratParser[TypeUsage] = positioned (
    "type" ~> ident
      ^^ (TypeUsage(_))
  )

  lazy val actionUsage: PackratParser[ActionUsage] = positioned (
    "action" ~> actionHeader
      ^^ (a => ActionUsage(a._1,a._2,a._3))
  )

  lazy val resolveBlock: PackratParser[ResolveBlock] = positioned (
    "resolve" ~ ident ~ "=" ~ libraryReference ~ resolveWith.?
      ^^ { case _~id~_~libRef~res => ResolveBlock(id,libRef,res match { case Some(x) => x; case None => Nil })}
  )

  lazy val resolveWith: PackratParser[List[ResolutionRule]] = (
    "with" ~ "{" ~ typeResolution.* ~ actionResolution.* ~ "}"
      ^^ { case _~_~tres~ares~_ => tres ::: ares }
  )

  lazy val typeResolution: PackratParser[TypeResolution] = positioned (
    "type" ~ ident ~ "=" ~ typeName
      ^^ { case _~id~_~tN => TypeResolution(id,tN) }
  )

  lazy val actionResolution: PackratParser[ActionResolution] = positioned (
    "action" ~ ident ~ "=" ~ libraryReference ~ "→" ~ ident
      ^^ { case _~localID~_~lib~_~libID => ActionResolution(localID,lib,libID) }
  )

  // Blocks and Statements

  lazy val block: PackratParser[List[Statement]] = (
    "{" ~ stmt.* ~ "}" ^^ {case _~a~_ => a}
  )

  lazy val stmt: PackratParser[Statement] = positioned (
    metaStmt | expressionStmt | assignStmt | ifStmt | whileStmt | forStmt | foreachStmt | skipStmt
  )

  lazy val metaStmt: PackratParser[MetaStatement] = positioned (
      "meta" ~ "private" ~ ";" ^^ (_ => MetaStatement("private",None))
    | "meta" ~ "recent" ~ "{" ~ ident.* ~ "}" ~ ";" ^^ {case _~_~_~ls~_~_ => MetaStatement("guid",Some(ls))}
    | "meta" ~ "guid" ~ stringLiteral ~ ";" ^^ {case _~_~guid~_ => MetaStatement("guid",Some(guid))}
  )

  // singleton is included here, since its allowed but is has no effect or returnval
  lazy val skipStmt:PackratParser[Skip] = positioned (
    (ident ~ ";" ||| "skip" ~ ";" ||| "..." ~ ";") ^^ (_ => Skip())
  )

  lazy val ifStmt: PackratParser[If] = positioned (
    "if" ~ expression ~ "then" ~ block ~ "else" ~ block ^^ {case _~e~_~b~_~c => If(e,b,c)}
    ||| "if" ~ expression ~ "then" ~ block ^^ {case _~e~_~b => If(e,b,Nil)}
  )

  lazy val whileStmt: PackratParser[While] = positioned (
    "while" ~ expression ~ "do" ~ block ^^ {case _~e~_~b => While(e,b)}
  )

  lazy val forStmt: PackratParser[For] = positioned (
    "for" ~ "0" ~ "≤" ~ ident ~ "<" ~ expression ~ "do" ~ block ^^ {case _~_~_~i~_~e~_~b => For(i,e,b)}
  )

  lazy val foreachStmt: PackratParser[Foreach] = positioned (
    "foreach" ~ ident ~ "in" ~ expression  ~ guard.* ~ "do" ~ block ^^ {case _~i~_~e~g~_~b => Foreach(i,e,g,b)}
  )

  lazy val guard: PackratParser[Expression] =
    "where" ~> expression

  lazy val expressionStmt: PackratParser[ExpressionStatement] = positioned (
    expression <~ ";" ^^ (x => ExpressionStatement(x))
  )

  lazy val assignStmt:  PackratParser[AssignStatement] = (
    parenthAssignStmt <~ ";"
  )

  lazy val parenthAssignStmt: PackratParser[AssignStatement] = positioned (
    (repsep(lvalue,",") <~ ":=") ~ expression ^^ {case l~r => AssignStatement(l,r)}
      ||| "(" ~> parenthAssignStmt <~ ")"
  )

  // Composed expressions with operator preferences

  lazy val expression: PackratParser[Expression] = orExpression

  lazy val orExpression: PackratParser[Expression] = positioned (
    andExpression ~ ("or" ~> orExpression).?
      ^^ {case left~Some(right) => Access(left,"or",List(right)); case left~None => left}
  )

  lazy val andExpression: PackratParser[Expression] = positioned (
    notExpression ~ ("and" ~> andExpression).?
      ^^ {case left~Some(right) => Access(left,"and",List(right)); case left~None => left}
  )

  lazy val notExpression: PackratParser[Expression] = positioned (
    ("not".r).* ~ comparisonExpression
      ^^ {case nots~expr => if (nots.length % 2 == 1) Access(expr,"not",List()) else expr }
  )

  lazy val comparisonExpression: PackratParser[Expression] = positioned (
    concatenationExpression ~ (( "=" | "≠" | "<" | "≤" | ">" | "≥") ~ concatenationExpression).?
      ^^ {case left~Some(op~right) => Access(left,op,List(right)); case left~None => left }
  )

  lazy val concatenationExpression: PackratParser[Expression] = positioned (
    addSubstrExpression ~ ("∥" ~> concatenationExpression).?
      ^^ {case left~Some(right) => Access(left,"∥",List(right)); case left~None => left}
  )

  lazy val addSubstrExpression: PackratParser[Expression] = positioned (
    multDivExpression ~ (("+" | "-") ~ addSubstrExpression).?
      ^^ {case left~Some(op~right) => Access(left,op,List(right)); case left~None => left }
  )

  lazy val multDivExpression: PackratParser[Expression] = positioned (
    negationExpression ~ (("*" | "/") ~ multDivExpression).?
      ^^ {case left~Some(op~right) => Access(left,op,List(right)); case left~None => left }
  )

  lazy val negationExpression: PackratParser[Expression] = positioned (
    "-" ~> negationExpression
      ^^ {case expr => Access(Literal(TypeName("Number"),"0"),"-",List(expr)) }
    ||| propertyAccess
  )

  lazy val propertyAccess: PackratParser[Expression] = positioned (
    propertyAccess ~ rA ~ ident ~ ("(" ~> repsep(expression,",") <~ ")").?
      ^^ { case ex~_~prop~args => args match { case None => Access(ex,prop,Nil); case Some(x) => Access(ex,prop,x) }}
    ||| terminalExpression
  )

  // Literals / References / Atomics

  lazy val terminalExpression: PackratParser[Expression] =
    ( "(" ~> expression <~ ")" | literalExpression | localReference | globalReference
      | singletonReference | libraryReference)

  lazy val lvalue: PackratParser[LValue] = localReference | globalReference

  lazy val localReference: PackratParser[LocalReference] = positioned (
    // This is a fix for 2.10. Somehow the plaintext represents collections and records as local variables
    "$" ~> ident ^^ ( s =>
      //if (s.startsWith("collections")) SingletonReference("collections")
      //
      LocalReference(s)
    )
  )

  lazy val globalReference: PackratParser[GlobalReference] = positioned (
    (("data" | "art") ~ rA) ~> ident ^^ (GlobalReference(_))
  )

  lazy val singletonReference: PackratParser[SingletonReference] = positioned (
    ident ^^ (SingletonReference(_))
  )

  lazy val libraryReference: PackratParser[LibraryReference] =  positioned (
    ("♻" ~> ident | "this" ) ^^ (LibraryReference(_))
  )

  lazy val literalExpression: PackratParser[Expression] = positioned (
    booleanLiteral ^^ (Literal(TypeName("Boolean"),_))
    ||| stringLiteral ^^ (Literal(TypeName("String"),_))
    ||| numberLiteral ^^ (Literal(TypeName("Number"),_))
  )

  // Tokens

  lazy val rA:PackratParser[String] = ("→" | "->")
  lazy val numberLiteral:PackratParser[String] = ( "[0-9]+\\.?".r ||| """[0-9]*\.[0-9]+""".r )
  lazy val booleanLiteral: Parser[String] = ("true" | "false")
  lazy val stringLiteral:PackratParser[String] = "\"" ~> escapedString <~ "\"" ^^ (StringEscapeUtils.unescapeJava(_))
  //lazy val escapedString: Parser[String] = """([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*""".r
  lazy val escapedString:Parser[String] = """(?:[^"\\]+|\\.)*""".r
  //lazy val escapedString:Parser[String] = """[^"\\]*(\\.[^"\\]*)*""".r
  lazy val ident: Parser[String] = """[a-zA-z@_](?:\w|\\.)*""".r ^^ (StringEscapeUtils.unescapeJava(_))

  /**
   * Ugly function to remove comments from the code. But works
   */
  def removeComments(input:String):String = {
    var s = input
    def skipUntil(c:Char,i:Int):Int = {
      var ret = i
      while (ret<s.length() && s.charAt(ret) != c) ret = ret + 1
      ret + 1
    }
    def skipUntilWithEscape(c:Char,i:Int):Int = {
      var ret = i
      while (ret<s.length() && (s.charAt(ret) != c || s.charAt(ret-1) == '\\')) ret = ret + 1
        ret + 1
    }
    var i = 0
    while (i < s.length) {
      if (s.charAt(i) == '"') i = skipUntilWithEscape('"',i+1)
      else {
        if (s.charAt(i) == '/' && i+1<s.length() && s.charAt(i+1) == '/') {
          val j = skipUntil('\n',i)
          s = s.substring(0,i)+s.substring(j-1,s.length)
        }
        i = i + 1
      }
    }
    s
  }

  // Parser Interface
  def apply(input: String):Script = {
    val noCommentInput = removeComments(input)
    parseAll(script,noCommentInput) match {
      case Success(x,_) => x
      case y:ParseResult[Script] => throw new TouchException("Parsing failed "+y)
    }
  }

}
