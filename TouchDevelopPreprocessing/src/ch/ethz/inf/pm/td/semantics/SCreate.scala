
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_SCreate
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of Create
 *
 * Create collections of items.
 *
 * @author Lucas Brutschy
 */ 

object SCreate extends Default_SCreate {

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** [**beta**] Creates an empty collection of arbitrary type */
    // case "collection of" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TUnfinished_Type)
    // DECLARATION AS FIELD: 
    //   /** [**beta**] Creates an empty collection of arbitrary type */
    //   lazy val field_collection_of = new TouchField("collection of",TUnfinished_Type.typeName)

    // FIELDS: field_collection_of

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
