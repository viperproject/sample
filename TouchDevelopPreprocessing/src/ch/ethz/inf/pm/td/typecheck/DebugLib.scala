package ch.ethz.inf.pm.td.typecheck

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

}
