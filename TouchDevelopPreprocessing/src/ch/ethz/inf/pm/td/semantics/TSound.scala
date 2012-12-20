
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichExpression._

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
  val field_pan = new TouchField("pan",TNumber.typ,toRichExpression(-1) ndTo toRichExpression(1))

  /** Gets the pitch adjustment, ranging from -1 (down one octave) to 1 (up one octave). */
  val field_pitch = new TouchField("pitch",TNumber.typ,toRichExpression(-1) ndTo toRichExpression(1))

  /** Gets the volume from 0 (silent) to 1 (full volume) */
  val field_volume = new TouchField("volume",TNumber.typ,toRichExpression(0) ndTo toRichExpression(1))

  val typName = "Sound"
  val typ = TouchType(typName,isSingleton = false,List(field_duration, field_pan, field_pitch, field_volume))

}

class TSound extends AAny {

  def getTyp = TSound.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Plays the sound effect */
    case "play" =>
      Skip

    /** Plays the song with different volume (0 to 1), pitch (-1 to 1) and pan (-1 to 1). */
    case "play_special" =>
      val List(volume,pitch,pan) = parameters // Number,Number,Number
      CheckInRangeInclusive[S](volume,0,1,"play_special","volume")
      CheckInRangeInclusive[S](pitch,-1,1,"play_special","pitch")
      CheckInRangeInclusive[S](pan,-1,1,"play_special","pan")
      Skip

    /** Sets the panning, ranging from -1.0 (full left) to 1.0 (full right). */
    case "set_pan" =>
       val List(pan) = parameters // Number
       CheckInRangeInclusive[S](pan,-1,1,"set_pan","pan")
       super.forwardSemantics(this0,method,parameters)

    /** Sets the pitch adjustment, ranging from -1 (down one octave) to 1 (up one octave). */
    case "set_pitch" =>
       val List(pitch) = parameters // Number
       CheckInRangeInclusive[S](pitch,-1,1,"set_pitch","pitch")
       super.forwardSemantics(this0,method,parameters)

    /** Sets the volume from 0 (silent) to 1 (full volume). */
    case "set_volume" =>
       val List(v) = parameters // Number
       CheckInRangeInclusive[S](v,0,1,"set_volume","volume")
       super.forwardSemantics(this0,method,parameters)

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
