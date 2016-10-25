package ch.ethz.inf.pm.sample.test

import ch.ethz.inf.pm.sample.oorepresentation.Compilable
import ch.ethz.inf.pm.sample.reporting.{SampleError, SampleInfo}
import ch.ethz.inf.pm.td.analysis.TouchDevelopAnalysisRunner
import org.scalatest.FunSuite

/**
  * Created by lucas on 25.08.16.
  */
class ComplexPoplWeakConsistencyTest extends FunSuite {

//  test("sky locale (Fixed) (pmzxrhbsrv)") {
//    runAnalysis("td://pmzxrhbsrv")
//  }
//
//  test("tetris (Fixed) (xrqeregnpk)") {
//    runAnalysis("td://xrqeregnpk")
//  }
//
//  test("TouchDatabase (cavke)") {
//    runAnalysis("td://cavke")
//  }

// === too Slow
//  test("Super Chat (cvuz)") {
//    runAnalysis("td://cvuz", Set(
//      ("assert.failed","FpIbCr712SAoC8HZ$i1"),
//      ("assert.failed","aS5Fzg0uNaT9R9W4$i3"),
//      ("assert.failed","xwmdQL1rEDZ0Vq4g$i5"),
//      ("assert.failed","Om82mYBUtlVEA5ky$i0"),
//      ("assert.failed","EERKdbqKd9eGXNtx$i1"),
//      ("assert.failed","xbPjF3KL6No8Dfq4$i0"),
//      ("assert.failed","ZZW64NpFdBvoExEJ$i0"),
//      ("assert.failed","Ke3LHOJVfoyktTu1$i0"),
//      ("assert.failed","xfRN4jYebyGwJPfQ$i0"),
//      ("assert.failed","IJIploCdYn69w0UX$i0")
//    ))
//  }

//// ==== Slow
//  test("Chatter box (fqaba)") {
//    runAnalysis("td://fqaba")
//  }

//  test("Hubstar (gbtxe)") {
//    runAnalysis("td://gbtxe")
//  }

//  test("tetris (gcane)") {
//    runAnalysis("td://gcane")
//  }

//// ==== Broken for some reason
//  test("HackER (kqfnc)") {
//    runAnalysis("td://kqfnc")
//  }

//// ==== Some harder bug
//  test("keyboard hero (ohgxa)") {
//    runAnalysis("td://ohgxa")
//  }

//// ==== Slow
//  test("sky locale (padg)") {
//    runAnalysis("td://padg")
//  }

//  test("metaverse (qnpge)") {
//    runAnalysis("td://qnpge")
//  }

//// ==== Some harder bug
//  test("TouchDevelop Jr. (qzeua)") {
//    runAnalysis("td://qzeua")
//  }

//// ==== Slow, but works!
//  test("cloud card (qzju)") {
//    runAnalysis("td://qzju")
//  }

//// ===== Slow
//  test("Relatd (ruef)") {
//    runAnalysis("td://ruef")
//  }

//// ===== Slow, but works.
//  test("Cloud Paper Scissors (sxjua)") {
//    runAnalysis("td://sxjua")
//  }

//  test("pentix (uvjba)") {
//    runAnalysis("td://uvjba")
//  }

//// === Slow, but works
//  test("Online Tic Tac Toe Multiplayer  (wccqepeb)") {
//    runAnalysis("td://wccqepeb")
//  }

//  test("Vulcanization calculator (whpgc)") {
//    runAnalysis("td://whpgc")
//  }

//// ==== Weird bug
//  test("Expense Splitter (wkvhc)") {
//    runAnalysis("td://wkvhc")
//  }

//// ==== Too slow for work.
//  test("guess multi-player demo (ycxbc)") {
//    runAnalysis("td://ycxbc")
//  }

  // Replace from Google Docs:
  //   +(\w+)\t([^\n]*)
  //  test("$2 ($1)") {\n    runAnalysis("td://$1")\n  }

  def runAnalysis(id:String, expectedErrors:Set[(String,String)] = Set.empty) = {
    val res = TouchDevelopAnalysisRunner.Default().run(Compilable.Identifier(id))
    val err =
      res.collect {
        case SampleError(i, _, pp, _) => (i, pp.toString)
        case SampleInfo(i, _, pp) => (i, pp.toString)
      }.toSet
    for (e <- expectedErrors) {
      assert(err.contains(e),"Did not report expected error "+e)
    }
    for (e <- err) {
      assert(expectedErrors.contains(e),"Did not expect reported error "+e)
    }
  }

}
