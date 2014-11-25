
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ExpressionInitializer, ApiField, RichNativeSemantics, TouchAnalysisParameters}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Sound
 *
 * A sound effect
 *
 * @author Lucas Brutschy
 */ 

object TSound extends AAny {

  /** Gets the duration in seconds. */
  lazy val field_duration = new ApiField("duration",TNumber.typeName)

  /** Gets the panning, ranging from -1.0 (full left) to 1.0 (full right). */
  lazy val field_pan = new ApiField("pan",TNumber.typeName,ExpressionInitializer(-1 ndTo 1))

  /** Gets the pitch adjustment, ranging from -1 (down one octave) to 1 (up one octave). */
  lazy val field_pitch = new ApiField("pitch",TNumber.typeName,ExpressionInitializer(-1 ndTo 1))

  /** Gets the volume from 0 (silent) to 1 (full volume) */
  lazy val field_volume = new ApiField("volume",TNumber.typeName,ExpressionInitializer(-1 ndTo 1))

  lazy val typeName = TypeName("Sound")

  override def possibleFields = super.possibleFields ++ List(field_duration, field_pan, field_pitch, field_volume)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Plays the sound effect */
    case "play" =>
      Skip

    /** Plays the song with different volume (0 to 1), pitch (-1 to 1) and pan (-1 to 1). */
    case "play special" =>
      val List(volume,pitch,pan) = parameters // Number,Number,Number
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](volume,0,1,"play special","volume")
        CheckInRangeInclusive[S](pitch,-1,1,"play special","pitch")
        CheckInRangeInclusive[S](pan,-1,1,"play special","pan")
      }
      Skip

    /** Sets the panning, ranging from -1.0 (full left) to 1.0 (full right). */
    case "set pan" =>
       val List(pan) = parameters // Number
       if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
         CheckInRangeInclusive[S](pan,-1,1,"set pan","pan")
       }
       super.forwardSemantics(this0,method,parameters,returnedType)

    /** Sets the pitch adjustment, ranging from -1 (down one octave) to 1 (up one octave). */
    case "set pitch" =>
       val List(pitch) = parameters // Number
       if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
         CheckInRangeInclusive[S](pitch,-1,1,"set pitch","pitch")
       }
       super.forwardSemantics(this0,method,parameters,returnedType)

    /** Sets the volume from 0 (silent) to 1 (full volume). */
    case "set volume" =>
       val List(v) = parameters // Number
       if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
         CheckInRangeInclusive[S](v,0,1,"set volume","volume")
       }
       super.forwardSemantics(this0,method,parameters,returnedType)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
