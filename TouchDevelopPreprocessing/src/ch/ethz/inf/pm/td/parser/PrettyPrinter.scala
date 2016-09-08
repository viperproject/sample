/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.parser

import org.apache.commons.lang3.StringEscapeUtils


/**
 *
 * Pretty prints an AST in a human-readable format
 *
 * @author Lucas Brutschy
 *
 */
object PrettyPrinter {

  val operators = List("+", "-", "*", "/", "and", "or", "not", ">", "<", "=", "≠", "≤", "≥", ":=")

  def apply(s: Script): String = {
    applyWithPPPrinter(s)({ (pp: IdPositional, s: String) => s })
  }

  def applyWithPPPrinter(s: Script)(implicit ppPrinter: ((IdPositional, String) => String)): String = {
    (s.declarations map apply).mkString("\n\n")
  }

  def apply(d: Declaration)(implicit ppPrinter: ((IdPositional, String) => String)): String = {
    ppPrinter(d,
      d match {
        case ActionType(ident, in, out, isPrivate) =>
          (if (isPrivate) "private " else "") +
            "action type" +
            apply(ident) + " (" + (in map apply).mkString(",") + ") returns " + (out map apply).mkString(",")
        case ActionDefinition(ident, in, out, body, isEvent, isPrivate) =>
          (if (isPrivate) "private " else "") +
            (if (isEvent) "event " else "action ") +
            apply(ident) + " (" + (in map apply).mkString(",") + ") returns " + (out map apply).mkString(",") +
            " {\n" + apply(body) + "\n}"
        case PageDefinition(ident, in, out, initBody, displayBody, isEvent, isPrivate) =>
          (if (isPrivate) "private " else "") + "page " +
            apply(ident) + " (" + (in map apply).mkString(",") + ") returns " + (out map apply).mkString(",") +
            " {\n" + apply(initBody) + apply(displayBody)  + "\n}" // TODO: This is not how /text does it
        case MetaDeclaration(ident, value) => "meta " + apply(ident) + " \"" + value + "\""
        case VariableDefinition(variable, map) => "var " + apply(variable) + " {" + map.map(x => x._1 + "=" + x._2 + ";").mkString("\n") + " }"
        case TableDefinition(ident, typeName, sourceName, keys, fields, isCloudEnabled, isCloudPartiallyEnabled, isPersistent, isExported) =>
          "table " + apply(ident) +
            " { type = \"" + typeName + "\"; " +
            (if (sourceName.nonEmpty) "sourceName = \"" + sourceName.get + "\"; " else "") +
            s"cloudenabled = $isCloudEnabled; cloudpartiallyenabled = $isCloudPartiallyEnabled; persistent = $isPersistent; exported = $isExported;" +
            "keys = {\n" + (keys map apply) + "\n} fields = {\n" + (fields map apply) + "\n} }"
        case LibraryDefinition(name, pub, _, _, _, _, usages, resolves) =>
          "meta import " + apply(name) + "{\n  pub \"" + pub + "\"\n  " +
            "usage " + "{\n    " + (usages map apply).mkString("\n    ") + "\n  }" + "\n  " +
            (resolves map apply).mkString("\n  ") + "\n}"
        case LibAbstractType(id) =>
          "abstract type " + apply(id)
      })
  }

  def apply(d: UsageDeclaration)(implicit ppPrinter: ((IdPositional, String) => String)): String = {
    ppPrinter(d,
      d match {
        case ActionUsage(ident, inParam, outParam) =>
          "action " + apply(ident) + " (" + (inParam map apply).mkString(",") + ") returns " + (outParam map apply).mkString(",")
      })
  }

  def apply(r: ResolveBlock)(implicit ppPrinter: ((IdPositional, String) => String)): String = {
    ppPrinter(r,
      "resolve " + apply(r.localName) + " = " + apply(r.libName) + " with { \n    " + (r.rules map apply).mkString("\n    ") + "}")
  }

  def apply(r: ResolutionRule)(implicit ppPrinter: ((IdPositional, String) => String)): String = {
    ppPrinter(r,
      r match {
        case TypeResolution(local, libName) => "type" + apply(local) + "=" + apply(libName)
        case ActionResolution(local, libName) => "action " + apply(local) + " = " + apply(libName)
      })
  }

  def apply(p: Parameter)(implicit ppPrinter: ((IdPositional, String) => String)): String = {
    ppPrinter(p, apply(p.ident) + ":" + apply(p.typeName))
  }

  def apply(s: List[Statement])(implicit ppPrinter: ((IdPositional, String) => String)): String = {
    s.map(apply).flatMap(_.split("\n")).map("  " + _).mkString("\n")
  }

  def apply(s: Statement)(implicit ppPrinter: ((IdPositional, String) => String)): String = {
    ppPrinter(s, s match {
      case For(idx, bnd, body) => "for (0 <= " + apply(idx) + " < " + apply(bnd) + ") {\n" + apply(body) + "\n}"
      case Foreach(elem, coll, _, body) => "foreach (" + apply(elem) + " in " + apply(coll) + ") {\n" + apply(body) + "\n}"
      case While(cond, body) => "while (" + apply(cond) + ") do {\n" + apply(body) + "\n}"
      case Box(body) => "do box {\n" + apply(body) + "\n}"
      case ExpressionStatement(expr) => apply(expr) + ";"
      case Skip() => "skip;"
      case MetaStatement(key, value) => "meta " + apply(key) + " \"" + value + "\";"
      case If(condition, thenBody, elseBody) => "if (" + apply(condition) + ") then {\n" + apply(thenBody) + "\n}" +
        (if (elseBody.nonEmpty) " else {\n" + apply(elseBody) + "\n}" else "")
      case WhereStatement(expr, handlers, optParams) => apply(expr) + (optParams map apply).mkString("") + (handlers map apply).mkString("")
      case Break() => "break;"
      case Continue() => "continue;"
      case Return(expr) => "return " + apply(expr) + ";"
      case Show(expr) => "show " + apply(expr) + ";"
    })
  }

  def apply(e: Expression)(implicit ppPrinter: ((IdPositional, String) => String)): String = {
    ppPrinter(e, e match {
      case Access(subject, property, args) =>
        if (operators.contains(property.ident)) "(" + apply(subject) + ") " + apply(property) + " (" + (args map apply).mkString(",") + ")"
        else apply(subject) + "->" + apply(property) + (if (args.isEmpty) "" else "(" + (args map apply).mkString(",") + ")")
      case LocalReference(ident) => "$" + apply(ident)
      case Placeholder(typ) => "$__optional_argument"
      case SingletonReference(ident, typ) => apply(ident)
      case Literal(typ, value) => typ match {
        case TypeName("String", _, _, _) => "\"" + value + "\""
        case _ => value
      }
    })
  }

  def apply(a: InlineAction)(implicit ppPrinter: ((IdPositional, String) => String)): String = {
    ppPrinter(a, " where " + apply(a.handlerName) + " (" +
      (a.inParameters map apply).mkString(",") + ") returns (" +
      (a.outParameters map apply).mkString(",") + ") {\n" + apply(a.body) + "\n}")
  }

  def apply(a: Identifier)(implicit ppPrinter: ((IdPositional, String) => String)): String = {
    ppPrinter(a, a.ident)
  }

  def apply(a: OptionalParameter)(implicit ppPrinter: ((IdPositional, String) => String)): String = {
    ppPrinter(a, " where " + apply(a.name) + " = " + apply(a.expr))
  }

  def apply(s: TypeName)(implicit ppPrinter: ((IdPositional, String) => String)): String = {
    ppPrinter(s, apply(s.ident))
  }

  def apply(s: String)(implicit ppPrinter: ((IdPositional, String) => String)): String = {
    if (s != null)
      StringEscapeUtils.escapeJava(s.replace(" ", "_"))
    else ""
  }

}
