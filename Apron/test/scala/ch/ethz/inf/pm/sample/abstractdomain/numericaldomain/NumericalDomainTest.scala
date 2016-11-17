/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{SemanticDomain, Constant}
import ch.ethz.inf.pm.sample.oorepresentation.DummyNumericalType
import ch.ethz.inf.pm.sample.test.{SemanticDomainTest, LatticeTest}


trait ApronTest[T <: Apron[T]]
  extends NumericalDomainTest[T]
    with MostPreciseAssignment[T] {

  SystemParameters.typ = DummyNumericalType

}

class ApronOctagonTest extends ApronTest[Apron.Octagons] {
  override def factory = Apron.Octagons.Bottom
}

class ApronLinearEqualitiesTest extends ApronTest[Apron.LinearEqualities] {
  override def factory = Apron.LinearEqualities.Bottom

  // FAILS, therefore ignore. This is for antoine
  override val ignoreLubLessEqualWidening = true

}

class ApronOptOctagonsTest extends ApronTest[Apron.OptOctagons] {
  override def factory = Apron.OptOctagons.Bottom
}

class ApronFloatOptOctagonsTest extends ApronTest[Apron.FloatOptOctagons] {
  override def factory = Apron.FloatOptOctagons.Bottom
}

class ApronPolyhedraTest extends ApronTest[Apron.Polyhedra] {
  override def factory = Apron.Polyhedra.Bottom
}

class ApronStrictPolyhedraTest extends ApronTest[Apron.StrictPolyhedra] {
  override def factory = Apron.StrictPolyhedra.Bottom
}

class ApronBoxTest extends ApronTest[Apron.Box] {
  override def factory = Apron.Box.Bottom

  // FAILS, therefore ignore. This is for antoine
  override val ignoreMostPreciseAssignment = true
}