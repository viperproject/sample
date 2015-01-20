package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.analysis.ApiField
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Used to represent keys for indexes with multiple keys as a single key
 *
 * @author Lucas Brutschy
 */
case class GTuple(types:List[AAny]) extends AAny {

  lazy val sortedKeyFields = types.zipWithIndex.map { x => ApiField("_"+x._2,x._1) }

  override lazy val typeName: TypeName = TypeName("Tuple",types.map(_.typeName))

  override lazy val possibleFields = super.possibleFields ++ sortedKeyFields

  override lazy val declarations = super.declarations ++ mkGetterSetters(sortedKeyFields)

}
