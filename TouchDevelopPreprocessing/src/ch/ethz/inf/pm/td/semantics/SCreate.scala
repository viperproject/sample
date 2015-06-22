
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler._
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

  override lazy val member_Collection_of = super.member_Collection_of.copy(
    returnType = TUnfinished_Collection,
    semantics = ValidPureSemantics
  )

  override lazy val member_Ref_of = super.member_Ref_of.copy(
    returnType = TUnfinished_Ref,
    semantics = ValidPureSemantics
  )

}
      
