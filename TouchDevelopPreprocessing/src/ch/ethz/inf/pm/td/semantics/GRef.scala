
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.analysis.ApiField
import ch.ethz.inf.pm.td.defsemantics.Default_GRef

/**
 * Customizes the abstract semantics of Ref
 *
 * A reference to a value
 *
 * @author Lucas Brutschy
 */
case class GRef (TT:AAny) extends Default_GRef {

//    lazy val field__ref = ApiField("◈ref",GRef(TT))
  lazy val field__get = ApiField("◈get",TT)
  lazy val field__confirmed = ApiField("◈confirmed",TBoolean)

  override lazy val possibleFields = super.possibleFields ++ Set(
//    field__ref,
    field__get,
    field__confirmed
  )

}
          
