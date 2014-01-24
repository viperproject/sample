package ch.ethz.inf.pm.sample.reporting

import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.sample.SystemParameters

object Reporter {

  var seenErrors = Set.empty[SampleError]
  var seenInfos = Set.empty[SampleInfo]
  var seenImprecision = Set[(String,ProgramPoint)]()
  var seenBottom = Set[(String,ProgramPoint)]()

  def hasError(err: SampleError):Boolean = seenErrors.contains(err)
  def hasInfo(info: SampleInfo):Boolean = seenInfos.contains(info)
  def hasImprecision(message:String,pp:ProgramPoint):Boolean = seenImprecision.contains((message,pp))
  def hasBottom(message:String,pp:ProgramPoint):Boolean = seenBottom.contains((message,pp))

  def getErrors(pp:ProgramPoint):Set[String] = seenErrors filter (_.pp == pp) map (_.message)
  def getImprecision(pp:ProgramPoint):Set[String] = seenImprecision.filter(_._2 == pp).map(_._1)
  def getBottom(pp:ProgramPoint):Set[String] = seenBottom.filter(_._2 == pp).map(_._1)

  def reportError(err: SampleError) {
    if (!hasError(err) && SystemParameters.enableOutputOfAlarms) {
      SystemParameters.progressOutput.put("ALARM: "+err.message+" at "+err.pp.toString)
      seenErrors += err
    }
  }

  def reportError(message:String,pp:ProgramPoint, id: String = "assert.failed") {
    val err = SampleError(id, message, pp)
    reportError(err)
  }

  def reportInfo(message:String,pp:ProgramPoint, id: String) {
    val info = SampleInfo(id, message, pp)
    if (!hasInfo(info) && SystemParameters.enableOutputOfAlarms) {
      SystemParameters.progressOutput.put("INFO: "+message+" at "+pp.toString)
      seenInfos += info
    }
  }

  def reportImprecision(message:String,pp:ProgramPoint) {
    if (!hasImprecision(message,pp) && SystemParameters.enableOutputOfPrecisionWarnings) {
      SystemParameters.progressOutput.put("PRECISION: "+message+" at "+pp.toString)
      seenImprecision += ((message,pp))
    }
  }

  def reportDummy(message:String,pp:ProgramPoint) {
    if (!hasImprecision(message,pp) && SystemParameters.enableOutputOfPrecisionWarnings && SystemParameters.enableOutputOfDummyWarnings) {
      SystemParameters.progressOutput.put("SOUND DUMMY: "+message+" at "+pp.toString)
      seenImprecision += ((message,pp))
    }
  }

  def reportBottom(message:String,pp:ProgramPoint) {
    if (!hasBottom(message,pp) && SystemParameters.enableOutputOfBottomWarnings) {
      SystemParameters.progressOutput.put("BOTTOM: "+message+" at "+pp.toString)
      seenBottom += ((message,pp))
    }
  }

  def reset {
    seenErrors = Set.empty[SampleError]
    seenInfos = Set.empty[SampleInfo]
    seenBottom = Set.empty[(String,ProgramPoint)]
    seenImprecision = Set.empty[(String,ProgramPoint)]
  }

}
