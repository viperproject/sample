package ch.ethz.inf.pm.td.parser

import org.apache.commons.lang3.StringEscapeUtils


/**
 *
 * Lucas Brutschy
 * Date: 8/23/12
 * Time: 6:54 PM
 *
 */
object PrettyPrinter {

  val operators = List("+", "-", "*", "/", "and", "or", "not", ">", "<", "=", "≠", "≤", "≥", ":=")

  def apply(s: Script)(implicit printTypes: Boolean): String = {
    (s.declarations map apply).mkString("\n\n")
  }

  def apply(d: Declaration)(implicit printTypes: Boolean): String = {
    d match {
      case ActionDefinition(ident, in, out, body, isEvent) =>
        (if (isEvent) "event " else "action ") +
          apply(ident) + " (" + (in map apply).mkString(",") + ") returns " + (out map apply).mkString(",") +
          " {\n" + apply(body) + "\n}"
      case MetaDeclaration(ident, value) => "meta " + apply(ident) + " \"" + value + "\""
      case VariableDefinition(variable, _) => "var " + apply(variable)
      case TableDefinition(ident, typName, _, _) => "table " + apply(ident) + " { type = \"" + typName + "\" }"
      case LibraryDefinition(name, pub, usages, resolves) =>
        "meta import " + apply(name) + "{\n  pub \"" + pub + "\"\n  " +
          "usage " + "{\n    " + (usages map apply).mkString("\n    ") + "\n  }" + "\n  " +
          (resolves map apply).mkString("\n  ") + "\n}"
      case PageDefinition(name, in, out, initBody, displayBody) =>
        "page " + apply(name) + " (" +
          (in map apply).mkString(",") + ") returns (" +
          (out map apply).mkString(",") + ") init {\n" +
          apply(initBody) + "\n} display {\n" +
          apply(displayBody) + "}"
    }
  }

  def apply(d: UsageDeclaration)(implicit printTypes: Boolean): String = {
    d match {
      case TypeUsage(ident) =>
        "type " + apply(ident)
      case ActionUsage(ident, inParam, outParam) =>
        "action " + apply(ident) + " (" + (inParam map apply).mkString(",") + ") returns " + (outParam map apply).mkString(",")
    }
  }

  def apply(r: ResolveBlock)(implicit printTypes: Boolean): String = {
    "resolve " + apply(r.localName) + " = " + apply(r.libName) + " with { \n    " + (r.rules map apply).mkString("\n    ") + "}"
  }

  def apply(r: ResolutionRule)(implicit printTypes: Boolean): String = {
    r match {
      case TypeResolution(local, libName) => "type" + apply(local) + "=" + apply(libName)
      case ActionResolution(local, libName) => "action " + apply(local) + " = " + apply(libName)
    }
  }

  def apply(p: Parameter)(implicit printTypes: Boolean): String = apply(p.ident) + ":" + apply(p.typeName)

  def apply(s: List[Statement])(implicit printTypes: Boolean): String = {
    ((s map apply map (_.split("\n")) flatten) map ("  " + _)).mkString("\n")
  }

  def apply(s: Statement)(implicit printTypes: Boolean): String = {
    s match {
      case For(idx, bnd, body) => "for (0 <= " + apply(idx) + " < " + apply(bnd) + ") {\n" + apply(body) + "\n}"
      case Foreach(elem, coll, _, body) => "foreach (" + apply(elem) + " in " + apply(coll) + ") {\n" + apply(body) + "\n}"
      case While(cond, body) => "while (" + apply(cond) + ") {\n" + apply(body) + "\n}"
      case Box(body) => "do box {\n" + apply(body) + "\n}"
      case ExpressionStatement(expr) => apply(expr) + ";"
      case Skip() => "skip;"
      case MetaStatement(key, value) => "meta " + apply(key) + " \"" + value + "\";"
      case If(condition, thenBody, elseBody) => "if (" + apply(condition) + ") {\n" + apply(thenBody) + "\n}" +
        (if (elseBody.length > 0) " else {\n" + apply(elseBody) + "\n}" else "")
      case WhereStatement(expr, handlers) => apply(expr) + (handlers map apply).mkString("")
    }
  }

  def apply(e: Expression)(implicit printTypes: Boolean): String = {
    e match {
      case Access(subject, Identifier(property), args) =>
        if (operators.contains(property)) "(" + apply(subject) + ") " + apply(property) + " (" + (args map apply).mkString(",") + ")"
        else apply(subject) + "->" + apply(property) + "(" + (args map apply).mkString(",") + ")"
      case LocalReference(ident) => "$" + apply(ident)
      case SingletonReference(ident, typ) => apply(ident)
      case Literal(typ, value) => typ match {
        case TypeName("String") => "\"" + value + "\""
        case _ => value
      }
    }
  }

  def apply(a: InlineAction)(implicit printTypes: Boolean): String = {
    " where " + apply(a.handlerName) + " (" +
      (a.inParameters map apply).mkString(",") + ") returns (" +
      (a.outParameters map apply).mkString(",") + ") {\n" + apply(a.body) + "\n}"
  }

  def apply(s: TypeName)(implicit printTypes: Boolean): String = {
    apply(s.ident)
  }

  def apply(s: String)(implicit printTypes: Boolean): String = {
    StringEscapeUtils.escapeJava(s.replace(" ", "_"))
  }

}
