package ch.ethz.inf.pm.td.transform

import ch.ethz.inf.pm.td.parser._
import ch.ethz.inf.pm.td.parser.LibraryDefinition
import ch.ethz.inf.pm.td.parser.Script

/**
 *
 * Lucas Brutschy
 * Date: 9/4/12
 * Time: 9:26 AM
 *
 */
object Linker {

  /**
   * Creates a linked script from any script (which might use libraries)
   *
   * @param scr The original script
   * @param pubToScript A function than fetches and pre-processes a script given a pubID. Used to load dependencies.
   * @return A script with only local calls and all required calls linked in
   */
  def apply(scr:Script, pubToScript: String => Script):Script = {

    val libs =
      (for(decl <- scr.declarations) yield {
        decl match {
          case LibraryDefinition(name,pubID,_,resolves) =>
            val lib = pubToScript(pubID)
            Some(LocalLinker(name,resolves).link(lib))
          case _ => None
        }
      }).flatten

    val main = LocalLinker("",Nil).link(scr)

    // Join all declarations, filter library definitions and meta stuff
    val allDeclarations = ((libs flatMap (x => x.declarations)) ::: main.declarations) filter (x => x match {
      case LibraryDefinition(_,_,_,_) => false
      case MetaDeclaration(_,_) => false
      case _ => true
    })

    scr.copy(declarations = allDeclarations)
  }

  /**
   * For a given script, this renames all top level elements and all references to
   * top level elements if required. This ensures that resolve clauses are correctly
   * handled and that there are no name clashes.
   *
   * @param curLib The name of the current library, empty if this is not a library
   * @param curResolves A list of rules how to resolve which library, which allows relatively free combination of libs
   *
   * TODO: Do we have to rewrite resolve rules?
   * TODO: Does TD allow non-exported types and global values?
   * TODO: This might fail for
   */
  case class LocalLinker(curLib:String, curResolves:List[ResolveBlock]) {

    def link(scr:Script):Script = {
      Script(scr.declarations map (link _))
    }

    private def link(decl:Declaration):Declaration = {
      decl match {
        case a@ActionDefinition(ident,in,out,body,isEvent) => ActionDefinition(makeThisIdent(ident),in,out,body map (link _),isEvent)
        case d@VariableDefinition(ident,flags) => VariableDefinition(Parameter(makeThisIdent(ident.ident),ident.typeName),flags)
        case x => x
      }
    }

    private def link(smt:Statement):Statement = {
      smt match {
        case ExpressionStatement(expr) => ExpressionStatement(link(expr))
        case AssignStatement(left,right) => AssignStatement((left map (link _)).asInstanceOf[List[LValue]],link(right))
        case For(loc,up,body) => For(loc,up,body map (link _))
        case While(cond,body) => While(cond,body map (link _))
        case Foreach(loc,coll,guards,body) => Foreach(loc,link(coll),guards map (link _),body map (link _))
        case If(cond,then,els) => If(link(cond),then map (link _),els map (link _))
        case x => x
      }
    }

    private def link(expr:Expression):Expression = {
      expr match {
        case Access(SingletonReference("code"),prop,args) => Access(SingletonReference("code"),makeThisIdent(prop),args map (link _))
        case Access(LibraryReference(id),prop,args) => Access(SingletonReference("code"),makeLibIdent(id,prop),args map (link _))
        case Access(subj,prop,args) => Access(link(subj),prop,args map (link _))
        case GlobalReference(ident) => GlobalReference(makeThisIdent(ident))
        case x => x
      }
    }

    /** Flat identifier for library references, which are eliminated and replaced by local calls */
    private def makeLibIdent(libName:String,ident:String) = "__"+libName+"_"+ident

    /** Flat identifier for local calls, which must be rewritten if this is a library */
    private def makeThisIdent(ident:String) = if(!curLib.isEmpty) makeLibIdent(curLib,ident) else ident

  }

}
