package ch.ethz.inf.pm.td.output

import ch.ethz.inf.pm.sample.Reporter
import java.io.{PrintWriter, FileWriter, File}
import ch.ethz.inf.pm.td.compiler.{TouchProgramPoint, TouchCompiler}

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

    for ((message,pp) <- Reporter.seenErrors) {
      pp match {
        case TouchProgramPoint(xScript, _) => if (xScript == id) res += "Error\t"+message+"\t"+pp else ""
        case _ => ""
      }
    }

    for ((message,pp) <- Reporter.seenBottom) {
      pp match {
        case TouchProgramPoint(xScript, _) => if (xScript == id) res += "Bottom\t"+message+"\t"+pp else ""
        case _ => ""
      }
    }

    for ((message,pp) <- Reporter.seenImprecision) {
      pp match {
        case TouchProgramPoint(xScript, _) => if (xScript == id) res += "Imprecision\t"+message+"\t"+pp else ""
        case _ => ""
      }
    }

    res

  }

}
