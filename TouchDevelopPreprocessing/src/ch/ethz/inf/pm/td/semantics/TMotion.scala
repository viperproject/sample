
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.ApiField
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TMotion
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of Motion
 *
 * Describes the motion of the device
 *
 * @author Lucas Brutschy
 */ 

object TMotion extends Default_TMotion {

  /** Gets the linear acceleration of the device, in gravitational units. */
  lazy val field_acceleration = ApiField("acceleration", TVector3)

  /** Gets the gravity vector associated with this reading. */
  lazy val field_gravity = ApiField("gravity", TVector3)

  /** Gets the pitch of the attitude in degrees */
  lazy val field_pitch = ApiField("pitch", TNumber)

  /** Gets the roll of the attitude in degrees */
  lazy val field_roll = ApiField("roll", TNumber)

  /** Gets the device rotation speed in degrees per sec. */
  lazy val field_rotation_speed = ApiField("rotation speed", TVector3)

  /** Gets a timestamp indicating the time at which the reading was calculated. */
  lazy val field_time = ApiField("time", TDateTime)

  /** Gets the yaw of the attitude in degrees */
  lazy val field_yaw = ApiField("yaw", TNumber)

  override def possibleFields = super.possibleFields ++ List(field_acceleration, field_gravity, field_pitch, field_roll,
    field_rotation_speed, field_time, field_yaw)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
