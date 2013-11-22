package ch.ethz.inf.pm.td.output

import ch.ethz.inf.pm.sample.Reporter
import java.io.{PrintWriter, FileWriter, File}

object TSVExporter {

  val file = File.createTempFile("testRunResults",".tsv")

  def apply() {


    if (file != null) {
      var fw:FileWriter = null
      var pw:PrintWriter = null
      try {

        fw = new FileWriter(file, true)
        pw = new PrintWriter(fw)

        for ((message,pp) <- Reporter.seenErrors) {
          pw.println("Error\t"+message+"\t"+pp)
        }
        for ((message,pp) <- Reporter.seenBottom) {
          pw.println("Bottom\t"+message+"\t"+pp)
        }
        for ((message,pp) <- Reporter.seenImprecision) {
          pw.println("Imprecision\t"+message+"\t"+pp)
        }

      } finally {

        if(pw != null) pw.close()
        if(fw != null) fw.close()

      }
      println("Analysis result have been written to: "+file.toURI.toString)
    } else {
      println("Failed to write analysis results")
    }

  }

}
