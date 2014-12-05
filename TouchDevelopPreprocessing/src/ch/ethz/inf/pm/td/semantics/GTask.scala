
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.defsemantics.Default_GTask

/**
 * Customizes the abstract semantics of Task
 *
 * A task created with `async` keyword
 *
 * @author Lucas Brutschy
 */

case class GTask (TT:AAny) extends Default_GTask {
          

//  lazy val field_value = ApiField("value",TT.typeName)
//  lazy val field_completed = ApiField("completed",TBoolean.typeName)
//  lazy val field_await = ApiField("await",TT.typeName)
                  

//  override lazy val possibleFields = super.possibleFields ++ Set(
//    field_value,
//    field_completed,
//    field_await
//  )
                  

}
          
