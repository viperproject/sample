
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Motion
 *
 * Describes the motion of the device
 *
 * @author Lucas Brutschy
 */ 

object TMotion {

  /** Gets the linear acceleration of the device, in gravitational units. */
  val field_acceleration = new TouchField("acceleration",TVector3.typ)

  /** Gets the gravity vector associated with this reading. */
  val field_gravity = new TouchField("gravity",TVector3.typ)

  /** Gets the pitch of the attitude in degrees */
  val field_pitch = new TouchField("pitch",TNumber.typ)

  /** Gets the roll of the attitude in degrees */
  val field_roll = new TouchField("roll",TNumber.typ)

  /** Gets the device rotation speed in degrees per sec. */
  val field_rotation_speed = new TouchField("rotation_speed",TVector3.typ)

  /** Gets a timestamp indicating the time at which the reading was calculated. */
  val field_time = new TouchField("time",TDateTime.typ)

  /** Gets the yaw of the attitude in degrees */
  val field_yaw = new TouchField("yaw",TNumber.typ)

  val typName = "Motion"
  val typ = new TouchType(typName,isSingleton = false, fields = List(field_acceleration, field_gravity, field_pitch, field_roll,
    field_rotation_speed, field_time, field_yaw))

}

class TMotion extends AAny {

  def getTyp = TMotion.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
