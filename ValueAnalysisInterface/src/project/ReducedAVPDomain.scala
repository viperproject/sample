package project

import sun.java2d.SunGraphicsEnvironment.T1Filter
import ch.ethz.inf.pm.sample.abstractdomain.{SemanticCartesianProductDomain, Lattice, ReducedSemanticProductDomain}

/**
 * Created by IntelliJ IDEA.
 * User: Pietro
 * Date: 30/03/12
 * Time: 13.24
 * To change this template use File | Settings | File Templates.
 */

abstract class ReducedAVPDomain[T1 <: AVPDomain[V1, T1], V1 <: Lattice[V1], T2 <: AVPDomain[V2, T2], V2 <: Lattice[V2], T <: ReducedAVPDomain[T1, V1, T2, V2, T]]
  (v1 : T1, v2 : T2) extends ReducedSemanticProductDomain[T1, T2, T](v1, v2) {


}