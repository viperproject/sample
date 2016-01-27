package ch.ethz.inf.pm.sample.test

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.td.domain.{BooleanInvalidDomainWithSource, InvalidExpression, ValidExpression}

/**
 * Does NOT have most precise assignment, as potentially Valid != Valid
 */
class BooleanInvalidDomainWithSourceTest
  extends SemanticDomainTest[BooleanInvalidDomainWithSource]{

  override lazy val values = super.values ++ Set(
    InvalidExpression(typ,"dummy1",DummyProgramPoint),
    ValidExpression(typ,DummyProgramPoint)
  )

  override lazy val typ = DummyNumericalType
  override lazy val factory = BooleanInvalidDomainWithSource()
}
