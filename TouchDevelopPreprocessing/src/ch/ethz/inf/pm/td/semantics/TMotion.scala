
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.ApiField
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of Motion
 *
 * Describes the motion of the device
 *
 * @author Lucas Brutschy
 */ 

object TMotion extends AAny {

  /** Gets the linear acceleration of the device, in gravitational units. */
  lazy val field_acceleration = new ApiField("acceleration",TVector3.typeName)

  /** Gets the gravity vector associated with this reading. */
  lazy val field_gravity = new ApiField("gravity",TVector3.typeName)

  /** Gets the pitch of the attitude in degrees */
  lazy val field_pitch = new ApiField("pitch",TNumber.typeName)

  /** Gets the roll of the attitude in degrees */
  lazy val field_roll = new ApiField("roll",TNumber.typeName)

  /** Gets the device rotation speed in degrees per sec. */
  lazy val field_rotation_speed = new ApiField("rotation speed",TVector3.typeName)

  /** Gets a timestamp indicating the time at which the reading was calculated. */
  lazy val field_time = new ApiField("time",TDateTime.typeName)

  /** Gets the yaw of the attitude in degrees */
  lazy val field_yaw = new ApiField("yaw",TNumber.typeName)

  lazy val typeName = TypeName("Motion")

  override def possibleFields = super.possibleFields ++ List(field_acceleration, field_gravity, field_pitch, field_roll,
    field_rotation_speed, field_time, field_yaw)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
