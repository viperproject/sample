package ch.ethz.inf.pm.td.parser


/**
 *
 * Lucas Brutschy
 * Date: 8/23/12
 * Time: 6:54 PM
 *
 */
object PrettyPrinter {

  val operators = List("+","-","*","/","and","or","not",">","<","=","≠","≤","≥")

  def apply(s:Script):String = (s.declarations map apply).mkString("\n\n")

  def apply(d:Declaration):String = { d match {
    case ActionDefinition(ident,in,out,body,isEvent) =>
      (if(isEvent) "event " else "action ")+
      ident+" ("+(in map apply).mkString(",")+") returns "+(out map apply).mkString(",")+
      " {\n"+apply(body)+"\n}"
    case MetaDeclaration(ident,value) => "meta "+ident+" \""+value+"\""
    case VariableDefinition(variable,_) => "var "+apply(variable)
    case TableDefinition(ident,typName,_,_) => "table "+ident+" { type = "+typName+" }"
    case LibraryDefinition(name,pub,usages,resolves) => "meta import " + name + "{\n  pub \"" + pub + "\"\n  " +
      "usage " + "{\n    " + (usages map apply).mkString("\n    ") + "\n  }" + "\n  " + (resolves map apply).mkString("\n  ") + "\n}"
  }}

  def apply(d:UsageDeclaration):String = { d match {
    case TypeUsage(ident) => "type " + ident
    case ActionUsage(ident,inParam,outParam) => "action "+ident+" ("+(inParam map apply).mkString(",")+") returns "+(outParam map apply).mkString(",")
  }}

  def apply(r:ResolveBlock):String = "resolve " + r.localName + " = " + apply(r.lib) + " with { \n    " + (r.rules map apply).mkString("\n    ") + "}"

  def apply(r:ResolutionRule):String = { r match {
    case TypeResolution(local,libName) => "type" + local + "=" + libName
    case ActionResolution(local,lib,libName) => "action " + local + " = " + apply(lib) + "→" + libName
  }}

  def apply(p:Parameter):String = p.ident+":"+p.typeName

  def apply(s:List[Statement]):String = ((s map apply map (_.split("\n")) flatten) map ("  "+_)).mkString("\n")

  def apply(s:Statement):String = { s match {
    case For(idx,bnd,body) => "for (0 <= "+idx+" < "+apply(bnd)+") {\n"+apply(body)+"\n}"
    case Foreach(elem,coll,_,body) => "foreach ("+elem+" in "+apply(coll)+") {\n"+apply(body)+"\n}"
    case While(cond,body) => "while ("+apply(cond)+") {\n"+apply(body)+"\n}"
    case AssignStatement(left,right) => (left map apply).mkString(",") + ":=" + apply(right) + ";"
    case ExpressionStatement(expr) => apply(expr) + ";"
    case Skip() => "skip;"
    case MetaStatement(key, value) => "meta "+key+" "+value+";"
    case If(condition,thenBody,elseBody) => "if ("+apply(condition)+") {\n"+apply(thenBody)+"\n}" +
      (if (elseBody.length > 0 ) " else {\n"+apply(elseBody)+"\n}" else "")
  }}

  def apply(e:Expression):String = { e match {
    case Access(subject,property,args) =>
      if(operators.contains(property)) "("+apply(subject)+") "+property+" ("+(args map apply).mkString(",")+")"
      else apply(subject)+"."+property+"("+(args map apply).mkString(",")+")"
    case LibraryReference(ident) => "libs."+ident
    case LocalReference(ident) => "$"+ident
    case GlobalReference(ident) => "data."+ident
    case SingletonReference(ident) => ident
    case Literal(typ,value) => typ match { case TypeName("String",None) =>  "\""+value+"\""; case _ => value }
  }}

}
