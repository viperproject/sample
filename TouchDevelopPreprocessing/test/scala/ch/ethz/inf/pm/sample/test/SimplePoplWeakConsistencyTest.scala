/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.test

import ch.ethz.inf.pm.sample.oorepresentation.Compilable
import ch.ethz.inf.pm.sample.reporting.{SampleError, SampleInfo}
import ch.ethz.inf.pm.td.analysis.{TouchAnalysisParameters, TouchDevelopAnalysisRunner}
import org.scalatest.FunSuite

/**
  * Created by lucas on 25.08.16.
  */
class SimplePoplWeakConsistencyTest extends FunSuite {

  test("dekker example (Fixed) (fekcblzqer)") {
    runAnalysis("td://fekcblzqer")
  }
  test("Events (Fixed) (hqttcleqlj)") {
    runAnalysis("td://hqttcleqlj",
      Set(
        ("assert.failed", "xnD7dfCiSuJKaAcF$i0"),
        ("assert.failed", "x1qJEOQgH4GLAno2$i3"),
        ("assert.failed", "zfjbjj2a8g0BbFjk$i3"),
        ("assert.failed", "vChL4v5wQ2WQzcAM$i3"),
        ("assert.failed", "xD0jFEce4yNrtjZu$i3"),
        ("assert.failed", "gY1IJwsNG9licFnp$i3"),
        ("assert.failed", "UuPke2O9n5X3PHwb$i3"),
        ("assert.failed", "UuPke2O9n5X3PHwb$i10")
      )
    )
  }
  test("cloud list (blqz)") {
    runAnalysis("td://blqz")
  }
  test("Save Passwords (eddm)") {
    // OK
    runAnalysis("td://eddm")
  }
  test("ec2 demo chat (eijba)") {
    runAnalysis("td://eijba")
  }
  test("NuvolaList 2 (kjxzcgcv)") {
    runAnalysis("td://kjxzcgcv")
  }
  test("FieldGPS (kmac)") {
    runAnalysis("td://kmac", Set(
      ("assert.failed", "xPBi9YDvwWi9x9gJ$i1"),
      ("assert.failed", "Opfcpo6ZUUsfJ20U$i0"),
      ("assert.failed", "hnSP4N2Kg8mkJOj1$i0"),
      ("assert.failed", "ZnqpY2DwXDPGjw3z$i0")
    ))
  }
  test("Cloud Example (kzwue)") {
    runAnalysis("td://kzwue")
  }
  test("instant poll (nggfa)") {
    runAnalysis("td://nggfa", Set(
      ("assert.failed", "eGYkEB9RPSeHXc7s$i0")
    ))
  }
  test("expense recorder (nvoha)") {
    runAnalysis("td://nvoha")
  }
  test("dekker example (oxhs)") {
    runAnalysis("td://oxhs")
  }
  test("Events (qwidc)") {
    runAnalysis("td://qwidc", Set(
      ("assert.failed", "xnD7dfCiSuJKaAcF$i0"),
      ("assert.failed", "x1qJEOQgH4GLAno2$i3"),
      ("assert.failed", "zfjbjj2a8g0BbFjk$i3"),
      ("assert.failed", "vChL4v5wQ2WQzcAM$i3"),
      ("assert.failed", "xD0jFEce4yNrtjZu$i3"),
      ("assert.failed", "gY1IJwsNG9licFnp$i3"),
      ("assert.failed", "UuPke2O9n5X3PHwb$i3"),
      ("assert.failed", "UuPke2O9n5X3PHwb$i10")
    ))
  }

  //  test("Contest Voting (etww)") {
  //    runAnalysis("td://etww")
  //  }
  //  test("Color Line (uvlma)") {
  //    runAnalysis("td://uvlma")
  //  }
  //  test("unique poll (wbuei)") {
  //    runAnalysis("td://wbuei")
  //  }

  // Replace from Google Docs:
  //   +(\w+)\t([^\n]*)
  //  test("$2 ($1)") {\n    runAnalysis("td://$1")\n  }

  def runAnalysis(id: String, expectedErrors: Set[(String, String)] = Set.empty): Unit = {
    val res = TouchDevelopAnalysisRunner.Default(
      TouchAnalysisParameters.get.copy(
        enableCloudAnalysis = true,
        conditionalHandlers = false,
        contextSensitiveInterproceduralAnalysis = false
      )
    ).run(Compilable.Identifier(id))
    val err =
      res.collect {
        case SampleError(i, _, pp, _) => (i, pp.toString)
        case SampleInfo(i, _, pp) => (i, pp.toString)
      }.toSet
    for (e <- expectedErrors) {
      assert(err.contains(e), "Did not report expected error " + e)
    }
    for (e <- err) {
      assert(expectedErrors.contains(e), "Did not expect reported error " + e)
    }
  }

}
