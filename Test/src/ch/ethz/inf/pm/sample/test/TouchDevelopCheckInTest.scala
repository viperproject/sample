package ch.ethz.inf.pm.sample.test

import org.scalatest.FlatSpec

/**
 * Run this before you do a check-in
 */
class TouchDevelopCheckInTest extends FlatSpec {

  "TouchBoost" should "have the same results for all scripts with aa" in {
     Run.main(List("-v","-p","Test/test/TouchDevelop/aa").toArray)
  }

  "TouchBoost" should "have the same results for all handconstructed scripts" in {
    Run.main(List("-v","-p","Test/test/TouchDevelop/handconstructed").toArray)
  }

  "TouchBoost" should "pass the apron tests" in {
     Run.main(List("-v","-p","Test/test/TouchDevelop/apron").toArray)
  }

  "TouchBoost" should "pass the interproc tests" in {
    Run.main(List("-v","-p","Test/test/TouchDevelop/interproc").toArray)
  }

  "TouchBoost" should "pass the summary tests" in {
    Run.main(List("-v","-p","Test/test/TouchDevelop/summary").toArray)
  }

  "TouchBoost" should "pass the PLDI RANDOM tests" in {
    Run.main(List("-v","-p","Test/test/TouchDevelop/pldi_random").toArray)
  }

  "TouchBoost" should "pass the PLDI TOP tests" in {
    Run.main(List("-v","-p","Test/test/TouchDevelop/pldi_top").toArray)
  }


}
