package ch.ethz.inf.pm.sample.test

import ch.ethz.inf.pm.sample.oorepresentation.Compilable
import ch.ethz.inf.pm.td.analysis.TouchRun
import ch.ethz.inf.pm.td.cloud.AbstractEventGraph
import org.scalatest.FunSuite

/**
  * @author Lucas Brutschy
  */
class BasicWeakConsistencyTest extends FunSuite {

  test("We should be able to write a cloud-connected application") {

    val program =
      """
        | action main() {
        |   data→x→\u25c8ref→\u25c8add(1);
        |   data->x := 1;
        | }
        |
        | var x : Number {
        |  cloudenabled = true;
        | }
        |
      """.stripMargin

    TouchRun.runInThread(Compilable.Code("Dekker", program))
    println(AbstractEventGraph.toString)

  }

}
