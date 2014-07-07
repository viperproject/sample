package ch.ethz.inf.pm.td.output

import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.sample.reporting.{Reporter, SampleError}
import ch.ethz.inf.pm.td.compiler.{SpaceSavingProgramPoint, TouchCompiler, TouchProgramPoint}
import com.mongodb.casbah.Imports._


object MongoExporter {

  lazy val client = {
    MongoClient()("tb")("analysisJobs")
  }

}

class MongoExporter extends ErrorExporter {

  def setDebugInformation(s: String) {

    MongoExporter.client.update(MongoDBObject("jobID" -> Exporters.jobID), $set("debug" -> s))

  }

  def setStatus(s: String) {

    MongoExporter.client.update(MongoDBObject("jobID" -> Exporters.jobID), $set("status" -> s))

  }

  def exportWarnings(compiler: TouchCompiler) {

    def getPP(pp: ProgramPoint): String = pp match {
      case touchPP: SpaceSavingProgramPoint => touchPP.fullPosString
      case touchPP: TouchProgramPoint => touchPP.fullPosString
      case _ => ""
    }

    val result = for (SampleError(id, message, pp, causes) <- Reporter.seenErrors) yield {
      MongoDBObject(
        "id" -> id,
        "message" -> message,
        "pp" -> getPP(pp),
        "causes" -> causes.map { x => MongoDBObject("message" -> x._1, "pp" -> getPP(x._2))}
      )
    }

    val html = new HTMLExporter().export(compiler.parsedTouchScripts)

    MongoExporter.client.update(MongoDBObject("jobID" -> Exporters.jobID), $set("result" -> result, "html" -> html))

  }

}
