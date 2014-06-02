package ch.ethz.inf.pm.td.output

import ch.ethz.inf.pm.td.compiler.{TouchProgramPoint, TouchCompiler}
import ch.ethz.inf.pm.sample.reporting.{Reporter, SampleError}

class TSVExporter extends ErrorExporter {

  def getExtension = "tsv"

  def apply(compiler: TouchCompiler): String = {
    (for ((id,_) <- compiler.parsedTouchScripts) yield apply(id)).mkString("\n")
  }

  def apply(compiler: TouchCompiler, id: String): String = {
    apply(id)
  }

  def apply(id:String):String = {

    var res = ""

    for (SampleError(id,message,pp,causes) <- Reporter.seenErrors) {
      pp match {
        case tpp: TouchProgramPoint => if (tpp.scriptID == id) res += "Error\t"+message+"\t"+pp else ""
        case _ => ""
      }
    }

    for ((message,pp) <- Reporter.seenBottom) {
      pp match {
        case tpp: TouchProgramPoint => if (tpp.scriptID == id) res += "Bottom\t"+message+"\t"+pp else ""
        case _ => ""
      }
    }

    for ((message,pp) <- Reporter.seenImprecision) {
      pp match {
        case tpp: TouchProgramPoint => if (tpp.scriptID == id) res += "Imprecision\t"+message+"\t"+pp else ""
        case _ => ""
      }
    }

    res

  }

}
