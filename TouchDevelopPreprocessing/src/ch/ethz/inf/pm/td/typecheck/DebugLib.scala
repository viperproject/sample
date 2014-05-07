package ch.ethz.inf.pm.td.typecheck

import ch.ethz.inf.pm.td.typecheck.GenericTypes._

/**
 *
 * What we add for debugging / implementation convenience
 *
 * Lucas Brutschy
 * Date: 8/20/12
 * Time: 5:45 PM
 *
 */
trait DebugLib extends AbstractSymbolTable {

  // an unknown value
  addType("Unknown", List(
    Member("∥",List("Unknown"),"String")
  ))

  // A collection of appointments
  addType("User Collection", gMutableCollection("User Collection", "User"))

  // A collection of appointments
  addType("Action Collection", gMutableCollection("Action Collection", "Action"))

  // A collection of appointments
  addType("Sound Collection", gMutableCollection("Sound Collection", "Sound"))

  // A collection of appointments
  addType("Picture Collection", gMutableCollection("Picture Collection", "Picture"))

  // A collection of appointments
  addType("Bluetooth Device Collection", gMutableCollection("Bluetooth Device Collection", "Bluetooth Device"))


}
