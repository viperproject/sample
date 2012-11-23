package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType

/**
 * User: lucas
 * Date: 11/8/12
 * Time: 6:10 PM
 */
object TLink {

  val field_address = new TouchField("address", TString.typ)
  val field_kind = new TouchField("kind", TString.typ)
  val field_location = new TouchField("location", TLocation.typ)

  val typName = "Link"
  val typ = TouchType(typName,isSingleton = false, List(field_address,field_kind,field_location))

}
