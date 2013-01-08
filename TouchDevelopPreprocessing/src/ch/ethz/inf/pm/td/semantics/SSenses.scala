package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of senses
 *
 * Camera, location, microphone and other sensors
 *
 * @author Lucas Brutschy
 */

object SSenses {

  val typName = "senses"
  val typ = TouchType(typName,isSingleton = true,List())

}

class SSenses extends AAny {

  def getTyp = SSenses.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    /** Gets filtered accelerometer data using a combination of a low-pass and threshold triggered high-pass on each axis to eliminate the majority of the sensor low amplitude noise while trending very quickly to large offsets (not perfectly smooth signal in that case), providing a very low latency. This is ideal for quickly reacting UI updates. */
    // case "acceleration_quick" =>
    //   Return[S](Valid(TVector3.typ))
    // DECLARATION AS FIELD:
    //   /** Gets filtered accelerometer data using a combination of a low-pass and threshold triggered high-pass on each axis to eliminate the majority of the sensor low amplitude noise while trending very quickly to large offsets (not perfectly smooth signal in that case), providing a very low latency. This is ideal for quickly reacting UI updates. */
    //   field_acceleration_quick = new TouchField("acceleration_quick",TVector3.typ)

    /** Gets filtered accelerometer data using a 1 Hz first-order low-pass on each axis to eliminate the main sensor noise while providing a medium latency. This can be used for moderately reacting UI updates requiring a very smooth signal. */
    // case "acceleration_smooth" =>
    //   Return[S](Valid(TVector3.typ))
    // DECLARATION AS FIELD:
    //   /** Gets filtered accelerometer data using a 1 Hz first-order low-pass on each axis to eliminate the main sensor noise while providing a medium latency. This can be used for moderately reacting UI updates requiring a very smooth signal. */
    //   field_acceleration_smooth = new TouchField("acceleration_smooth",TVector3.typ)

    /** Gets filtered and temporally averaged accelerometer data using an arithmetic mean of the last 25 'optimally filtered' samples, so over 500ms at 50Hz on each axis, to virtually eliminate most sensor noise. This provides a very stable reading but it has also a very high latency and cannot be used for rapidly reacting UI. */
    // case "acceleration_stable" =>
    //   Return[S](Valid(TVector3.typ))
    // DECLARATION AS FIELD:
    //   /** Gets filtered and temporally averaged accelerometer data using an arithmetic mean of the last 25 'optimally filtered' samples, so over 500ms at 50Hz on each axis, to virtually eliminate most sensor noise. This provides a very stable reading but it has also a very high latency and cannot be used for rapidly reacting UI. */
    //   field_acceleration_stable = new TouchField("acceleration_stable",TVector3.typ)

    /** Gets the primary camera if available */
    // case "camera" =>
    //   Return[S](Valid(TCamera.typ))
    // DECLARATION AS FIELD:
    //   /** Gets the primary camera if available */
    //   field_camera = new TouchField("camera",TCamera.typ)

    /** Gets the current phone location. The phone optimizes the accuracy for power, performance, and other cost considerations. */
    // case "current_location" =>
    //   Return[S](Valid(TLocation.typ))
    // DECLARATION AS FIELD:
    //   /** Gets the current phone location. The phone optimizes the accuracy for power, performance, and other cost considerations. */
    //   field_current_location = new TouchField("current_location",TLocation.typ)

    /** Gets the current phone location with the most accuracy. This includes using services that might charge money,
      * or consuming higher levels of battery power or connection bandwidth. */
     case "current_location_accurate" =>
       New[S](TLocation.typ)

    /** Gets the front facing camera if available */
    // case "front_camera" =>
    //   Return[S](Valid(TCamera.typ))
    // DECLARATION AS FIELD:
    //   /** Gets the front facing camera if available */
    //   field_front_camera = new TouchField("front_camera",TCamera.typ)

    /** DEPRECATED. Test if the senses→acceleration quick is invalid instead */
    case "has_accelerometer" =>
      Return[S](Environment.hasAccelerometer)

    /** DEPRECATED. Test if the senses→heading is invalid instead */
    case "has_compass" =>
      Return[S](Environment.hasCompass)

    /** DEPRECATED. Test if the senses→front camera is invalid instead */
    case "has_front_camera" =>
      Return[S](Environment.hasFrontCamera)

    /** Indicates if the gyroscope is available on the device */
    case "has_gyroscope" =>
      Return[S](Environment.hasGyroscope)

    /** DEPRECATED. Test if the senses→motion is invalid instead. */
    case "has_motion" =>
      Return[S](RichExpression(Environment.hasAccelerometer)
        && RichExpression(Environment.hasCompass)
        && RichExpression(Environment.hasGyroscope))

    /** Gets the compass heading, in degrees, measured clockwise from the Earth's geographic north. */
     case "heading" =>
       Error[S](RichExpression(Environment.hasCompass).not(),"retrieving heading requires checking for a compass first!")
       Return[S](Valid(TNumber.typ))

    /** Indicates whether the device is 'stable' (no movement for about 0.5 seconds) */
    // case "is_device_stable" =>
    //   Return[S](Valid(TBoolean.typ))
    // DECLARATION AS FIELD:
    //   /** Indicates whether the device is 'stable' (no movement for about 0.5 seconds) */
    //   field_is_device_stable = new TouchField("is_device_stable",TBoolean.typ)

    /** Gets the current motion that combines data from the accelerometer, compass and gyroscope if available. */
    case "motion" =>
      Error((RichExpression(Environment.hasAccelerometer)
        && RichExpression(Environment.hasCompass)
        && RichExpression(Environment.hasGyroscope)).not(),
        "The mobile phone might not have the correct capabilities for this!")(state,pp)
      Skip

    /** Gets the current orientation in degrees if available. (x,y,z) is also called (pitch, roll, yaw) or (alpha, beta, gamma). */
    // case "orientation" =>
    //   Return[S](Valid(TVector3.typ))
    // DECLARATION AS FIELD:
    //   /** Gets the current orientation in degrees if available. (x,y,z) is also called (pitch, roll, yaw) or (alpha, beta, gamma). */
    //   field_orientation = new TouchField("orientation",TVector3.typ)

    /** Records audio using the microphone */
    // case "record_microphone" =>
    //   Return[S](Valid(TSound.typ))
    // DECLARATION AS FIELD:
    //   /** Records audio using the microphone */
    //   field_record_microphone = new TouchField("record_microphone",TSound.typ)

    /** Gets the gyroscope rotational velocity around each axis of the device, in degrees per second. */
    // case "rotation_speed" =>
    //   Return[S](Valid(TVector3.typ))
    // DECLARATION AS FIELD:
    //   /** Gets the gyroscope rotational velocity around each axis of the device, in degrees per second. */
    //   field_rotation_speed = new TouchField("rotation_speed",TVector3.typ)

    /** Takes a picture and returns it. This picture does not contain the gps location. */
    // case "take_camera_picture" =>
    //   Return[S](Valid(TPicture.typ))
    // DECLARATION AS FIELD:
    //   /** Takes a picture and returns it. This picture does not contain the gps location. */
    //   field_take_camera_picture = new TouchField("take_camera_picture",TPicture.typ)


    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }

}