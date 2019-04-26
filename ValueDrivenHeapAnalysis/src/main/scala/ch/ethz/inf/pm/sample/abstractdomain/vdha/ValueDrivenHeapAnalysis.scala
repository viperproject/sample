/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, Analysis}
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Apron
import ch.ethz.inf.pm.sample.oorepresentation.NativeMethodSemantics
import ch.ethz.inf.pm.sample.oorepresentation.scalalang.{BooleanNativeMethodSemantics, IntegerNativeMethodSemantics, ObjectNativeMethodSemantics}
import ch.ethz.inf.pm.sample.property.Property

class ValueDrivenHeapAnalysis extends Analysis {

  def getLabel(): String = "Value Driven Heap analysis"

  def parameters(): List[(String, Any)] = Nil

  def setParameter(label: String, value: Any) {
    //    label match {
    //      case "Domain" => value match {
    //        case "Interval" => domain = new Box()
    //        case "PPL" => domain = new Polka(false) //new PplPoly(false); FIXIT: Change back to PPL
    //        case "Octagons" => domain = new Octagon()
    //        case "Polka" => domain = new Polka(false)
    //        case "Linear equalities" => domain = new PolkaEq()
    //      }
    //    }
  }

  def reset() {}

  def getInitialState: ValueDrivenHeapState.Default[Apron.Polyhedra] = {
    val generalValState = Apron.Polyhedra.Top.factory()
    ValueDrivenHeapState.Default(new HeapGraph[Apron.Polyhedra](), generalValState, ExpressionSet())
  }

  def getProperties: List[Property] = List(
    //    new ShowGraphProperty().asInstanceOf[Property]
    //    new SingleStatementProperty(DivisionByZero),
    //    new SingleStatementProperty(new LowerBoundedValue("y", 0)),
    //    new SingleStatementProperty(new BoundedValue("y", -4, 4))
  )

  def getNativeMethodsSemantics: List[NativeMethodSemantics] =
    List(ObjectNativeMethodSemantics, IntegerNativeMethodSemantics, BooleanNativeMethodSemantics)

}