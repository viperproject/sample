package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.parser.TypeName

object TUnfinished_Ref extends AAny {

  override lazy val typeName: TypeName = TypeName("Unfinished Ref")

  override def getDeclaration(s:String) =
    TypeList.getType(TypeName(s)) match {
      case Some(x) => Some(ApiMember(s,Nil,ApiParam(this),GRef(x),NewSemantics))
      case None => super.getDeclaration(s)
    }

}