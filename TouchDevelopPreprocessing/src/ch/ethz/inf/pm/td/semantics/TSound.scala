
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Sound
 *
 * A sound effect
 *
 * @author Lucas Brutschy
 */ 

object TSound {

  /** Gets the duration in seconds. */
  val field_duration = new TouchField("duration",TNumber.typ)

  /** Gets the panning, ranging from -1.0 (full left) to 1.0 (full right). */
  val field_pan = new TouchField("pan",TNumber.typ,ExpressionInitializer(-1 ndTo 1))

  /** Gets the pitch adjustment, ranging from -1 (down one octave) to 1 (up one octave). */
  val field_pitch = new TouchField("pitch",TNumber.typ,ExpressionInitializer(-1 ndTo 1))

  /** Gets the volume from 0 (silent) to 1 (full volume) */
  val field_volume = new TouchField("volume",TNumber.typ,ExpressionInitializer(-1 ndTo 1))

  val typName = "Sound"
  val typ = new TouchType(typName,isSingleton = false, fields = List(field_duration, field_pan, field_pitch, field_volume))

}

class TSound extends AAny {

  def getTyp = TSound.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Plays the sound effect */
    case "play" =>
      Skip

    /** Plays the song with different volume (0 to 1), pitch (-1 to 1) and pan (-1 to 1). */
    case "play special" =>
      val List(volume,pitch,pan) = parameters // Number,Number,Number
      CheckInRangeInclusive[S](volume,0,1,"play special","volume")
      CheckInRangeInclusive[S](pitch,-1,1,"play special","pitch")
      CheckInRangeInclusive[S](pan,-1,1,"play special","pan")
      Skip

    /** Sets the panning, ranging from -1.0 (full left) to 1.0 (full right). */
    case "set pan" =>
       val List(pan) = parameters // Number
       CheckInRangeInclusive[S](pan,-1,1,"set pan","pan")
       super.forwardSemantics(this0,method,parameters,returnedType)

    /** Sets the pitch adjustment, ranging from -1 (down one octave) to 1 (up one octave). */
    case "set pitch" =>
       val List(pitch) = parameters // Number
       CheckInRangeInclusive[S](pitch,-1,1,"set pitch","pitch")
       super.forwardSemantics(this0,method,parameters,returnedType)

    /** Sets the volume from 0 (silent) to 1 (full volume). */
    case "set volume" =>
       val List(v) = parameters // Number
       CheckInRangeInclusive[S](v,0,1,"set volume","volume")
       super.forwardSemantics(this0,method,parameters,returnedType)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
