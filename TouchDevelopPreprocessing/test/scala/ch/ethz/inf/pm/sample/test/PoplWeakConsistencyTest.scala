package ch.ethz.inf.pm.sample.test

import ch.ethz.inf.pm.sample.oorepresentation.Compilable
import ch.ethz.inf.pm.td.analysis.TouchDevelopAnalysisRunner
import org.scalatest.FunSuite

/**
  * Created by lucas on 25.08.16.
  */
class PoplWeakConsistencyTest extends FunSuite {

//  test("dekker example (Fixed) (fekcblzqer)") {
//    runAnalysis("td://fekcblzqer")
//  }
//  test("Events (Fixed) (hqttcleqlj)") {
//    runAnalysis("td://hqttcleqlj")
//  }
//  test("CSCE 1030 DASHBOARD (Fixed) (nekvbalhdm)") {
//    runAnalysis("td://nekvbalhdm")
//  }
////  test("sky locale (Fixed) (pmzxrhbsrv)") {
////    runAnalysis("td://pmzxrhbsrv")
////  }
////  test("tetris (Fixed) (xrqeregnpk)") {
////    runAnalysis("td://xrqeregnpk")
////  }
//  test("cloud list (blqz)") {
//    runAnalysis("td://blqz")
//  }
////  test("TouchDatabase (cavke)") {
////    runAnalysis("td://cavke")
////  }
//  test("Super Chat (cvuz)") {
//    runAnalysis("td://cvuz")
//  }
//  test("Save Passwords (eddm)") {
//    runAnalysis("td://eddm")
//  }
//  test("ec2 demo chat (eijba)") {
//    runAnalysis("td://eijba")
//  }
//  test("Contest Voting (etww)") {
//    runAnalysis("td://etww")
//  }
//  test("Chatter box (fqaba)") {
//    runAnalysis("td://fqaba")
//  }
////  test("Hubstar (gbtxe)") {
////    runAnalysis("td://gbtxe")
////  }
////  test("tetris (gcane)") {
////    runAnalysis("td://gcane")
////  }
//  test("NuvolaList 2 (kjxzcgcv)") {
//    runAnalysis("td://kjxzcgcv")
//  }
//  test("FieldGPS (kmac)") {
//    runAnalysis("td://kmac")
//  }
//  test("HackER (kqfnc)") {
//    runAnalysis("td://kqfnc")
//  }
//  test("Cloud Example (kzwue)") {
//    runAnalysis("td://kzwue")
//  }
//  test("instant poll (nggfa)") {
//    runAnalysis("td://nggfa")
//  }
//  test("expense recorder (nvoha)") {
//    runAnalysis("td://nvoha")
//  }
//  test("keyboard hero (ohgxa)") {
//    runAnalysis("td://ohgxa")
//  }
//  test("CSCE 1030 DASHBOARD (ornb)") {
//    runAnalysis("td://ornb")
//  }
//  test("dekker example (oxhs)") {
//    runAnalysis("td://oxhs")
//  }
//  test("sky locale (padg)") {
//    runAnalysis("td://padg")
//  }
////  test("metaverse (qnpge)") {
////    runAnalysis("td://qnpge")
////  }
//  test("Events (qwidc)") {
//    runAnalysis("td://qwidc")
//  }
//  test("TouchDevelop Jr. (qzeua)") {
//    runAnalysis("td://qzeua")
//  }
//  test("cloud card (qzju)") {
//    runAnalysis("td://qzju")
//  }
//  test("Relatd (ruef)") {
//    runAnalysis("td://ruef")
//  }
//  test("Cloud Paper Scissors (sxjua)") {
//    runAnalysis("td://sxjua")
//  }
////  test("pentix (uvjba)") {
////    runAnalysis("td://uvjba")
////  }
//  test("Color Line (uvlma)") {
//    runAnalysis("td://uvlma")
//  }
//  test("unique poll (wbuei)") {
//    runAnalysis("td://wbuei")
//  }
//  test("Online Tic Tac Toe Multiplayer  (wccqepeb)") {
//    runAnalysis("td://wccqepeb")
//  }
////  test("Vulcanization calculator (whpgc)") {
////    runAnalysis("td://whpgc")
////  }
//  test("Expense Splitter (wkvhc)") {
//    runAnalysis("td://wkvhc")
//  }
//  test("guess multi-player demo (ycxbc)") {
//    runAnalysis("td://ycxbc")
//  }

  // Replace from Google Docs:
  //   +(\w+)\t([^\n]*)
  //  test("$2 ($1)") {\n    runAnalysis("td://$1")\n  }

  def runAnalysis(id:String) = {
     TouchDevelopAnalysisRunner.Default().run(Compilable.Identifier(id))
  }

}
