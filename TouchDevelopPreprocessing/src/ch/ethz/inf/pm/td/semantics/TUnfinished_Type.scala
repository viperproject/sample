package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{DefaultSemantics, ApiParam, ApiMember, TypeList}
import ch.ethz.inf.pm.td.parser.TypeName

object TUnfinished_Type extends AAny {

  override lazy val typeName: TypeName = TypeName("Unfinished Type")

  override def getDeclaration(s:String) =
    TypeList.getType(TypeName(s)) match {
      case Some(x) => Some(ApiMember(s,Nil,ApiParam(this),x,DefaultSemantics))
      case None => super.getDeclaration(s)
    }

}
