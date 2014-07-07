package ch.ethz.inf.pm.td.output

import ch.ethz.inf.pm.sample.reporting.{Reporter, SampleError}
import ch.ethz.inf.pm.td.compiler.{SpaceSavingProgramPoint, TouchCompiler, TouchProgramPoint, TouchProgramPointRegistry}

class TSVExporter extends FileSystemExporter {

  def getExtension = "tsv"

  def warningsToString(compiler: TouchCompiler): String = {
    (for ((id, _) <- compiler.parsedTouchScripts) yield apply(id)).mkString("\n")
  }

  def warningsToString(compiler: TouchCompiler, id: String): String = {
    apply(id)
  }

  def apply(id: String): String = {

    var res = ""

    for (SampleError(_, message, pp, causes) <- Reporter.seenErrors) {
      pp match {
        case touchPP: SpaceSavingProgramPoint =>
          val tpp = TouchProgramPointRegistry.reg(touchPP.id)
          if (id == tpp.scriptID) "Error\t" + message + "\t" + pp else ""
        case tpp: TouchProgramPoint => if (tpp.scriptID == id) res += "Error\t" + message + "\t" + pp else ""
        case _ => ""
      }
    }

    for ((message, pp) <- Reporter.seenBottom) {
      pp match {
        case touchPP: SpaceSavingProgramPoint =>
          val tpp = TouchProgramPointRegistry.reg(touchPP.id)
          if (id == tpp.scriptID) "Error\t" + message + "\t" + pp else ""
        case tpp: TouchProgramPoint => if (tpp.scriptID == id) res += "Bottom\t" + message + "\t" + pp else ""
        case _ => ""
      }
    }

    for ((message, pp) <- Reporter.seenImprecision) {
      pp match {
        case touchPP: SpaceSavingProgramPoint =>
          val tpp = TouchProgramPointRegistry.reg(touchPP.id)
          if (id == tpp.scriptID) "Error\t" + message + "\t" + pp else ""
        case tpp: TouchProgramPoint => if (tpp.scriptID == id) res += "Imprecision\t" + message + "\t" + pp else ""
        case _ => ""
      }
    }

    res

  }

}
