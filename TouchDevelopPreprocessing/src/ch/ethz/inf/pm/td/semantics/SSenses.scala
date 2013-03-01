package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{Identifier, ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of senses
 *
 * Camera, location, microphone and other sensors
 *
 * @author Lucas Brutschy
 */

object SSenses {

  /** DEPRECATED. Test if the senses→acceleration quick is invalid instead */
  val field_has_accelerometer = new TouchField("has_accelerometer",TBoolean.typ)

  /** DEPRECATED. Test if the senses→heading is invalid instead */
  val field_has_compass = new TouchField("has_compass",TBoolean.typ)

  /** DEPRECATED. Test if the senses→front camera is invalid instead */
  val field_has_front_camera = new TouchField("has_front_camera",TBoolean.typ)

  /** Indicates if the gyroscope is available on the device */
  val field_has_gyroscope = new TouchField("has_gyroscope",TBoolean.typ)

  val typName = "senses"
  val typ = new TouchType(typName,isSingleton = true,
    List(field_has_accelerometer,field_has_compass,field_has_front_camera,field_has_gyroscope))

}

class SSenses extends AAny {

  def getTyp = SSenses.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    /** Gets filtered accelerometer data using a combination of a low-pass and threshold triggered high-pass on each axis to eliminate the majority of the sensor low amplitude noise while trending very quickly to large offsets (not perfectly smooth signal in that case), providing a very low latency. This is ideal for quickly reacting UI updates. */
    case "acceleration_quick" =>
       Error[S](Field[S](this0,SSenses.field_has_accelerometer).not(),"retrieving acceleration requires checking for a accelerometer first!")
       New[S](TVector3.typ,Map(TVector3.field_x.asInstanceOf[Identifier] -> Valid(TNumber.typ),TVector3.field_y -> Valid(TNumber.typ),TVector3.field_z -> Valid(TNumber.typ)))

    /** Gets filtered accelerometer data using a 1 Hz first-order low-pass on each axis to eliminate the main sensor noise while providing a medium latency. This can be used for moderately reacting UI updates requiring a very smooth signal. */
    case "acceleration_smooth" =>
       Error[S](Field[S](this0,SSenses.field_has_accelerometer).not(),"retrieving acceleration requires checking for a accelerometer first!")
       New[S](TVector3.typ,Map(TVector3.field_x.asInstanceOf[Identifier] -> Valid(TNumber.typ),TVector3.field_y -> Valid(TNumber.typ),TVector3.field_z -> Valid(TNumber.typ)))

    /** Gets filtered and temporally averaged accelerometer data using an arithmetic mean of the last 25 'optimally filtered' samples, so over 500ms at 50Hz on each axis, to virtually eliminate most sensor noise. This provides a very stable reading but it has also a very high latency and cannot be used for rapidly reacting UI. */
    case "acceleration_stable" =>
       Error[S](Field[S](this0,SSenses.field_has_accelerometer).not(),"retrieving acceleration requires checking for a accelerometer first!")
       New[S](TVector3.typ,Map(TVector3.field_x.asInstanceOf[Identifier] -> Valid(TNumber.typ),TVector3.field_y -> Valid(TNumber.typ),TVector3.field_z -> Valid(TNumber.typ)))

    /** Gets the primary camera if available */
    // case "camera" =>
    //   Top[S](TCamera.typ)
    // DECLARATION AS FIELD:
    //   /** Gets the primary camera if available */
    //   field_camera = new TouchField("camera",TCamera.typ)

    /** Gets the current phone location. The phone optimizes the accuracy for power, performance, and other cost considerations. */
    // case "current_location" =>
    //   Top[S](TLocation.typ)
    // DECLARATION AS FIELD:
    //   /** Gets the current phone location. The phone optimizes the accuracy for power, performance, and other cost considerations. */
    //   field_current_location = new TouchField("current_location",TLocation.typ)

    /** Gets the current phone location with the most accuracy. This includes using services that might charge money,
      * or consuming higher levels of battery power or connection bandwidth. */
     case "current_location_accurate" =>
       New[S](TLocation.typ)

    /** Gets the front facing camera if available */
    // case "front_camera" =>
    //   Top[S](TCamera.typ)
    // DECLARATION AS FIELD:
    //   /** Gets the front facing camera if available */
    //   field_front_camera = new TouchField("front_camera",TCamera.typ)

    /** DEPRECATED. Test if the senses→motion is invalid instead. */
    case "has_motion" =>
      Return[S](Field[S](this0,SSenses.field_has_accelerometer)
        && Field[S](this0,SSenses.field_has_compass)
        && Field[S](this0,SSenses.field_has_gyroscope))

    /** Gets the compass heading, in degrees, measured clockwise from the Earth's geographic north. */
     case "heading" =>
       Error[S](Field[S](this0,SSenses.field_has_compass).not(),"retrieving heading requires checking for a compass first!")
       Top[S](TNumber.typ)

    /** Indicates whether the device is 'stable' (no movement for about 0.5 seconds) */
    // case "is_device_stable" =>
    //   Top[S](TBoolean.typ)
    // DECLARATION AS FIELD:
    //   /** Indicates whether the device is 'stable' (no movement for about 0.5 seconds) */
    //   field_is_device_stable = new TouchField("is_device_stable",TBoolean.typ)

    /** Gets the current motion that combines data from the accelerometer, compass and gyroscope if available. */
    case "motion" =>
      Error((Field[S](this0,SSenses.field_has_accelerometer)
        && Field[S](this0,SSenses.field_has_compass)
        && Field[S](this0,SSenses.field_has_gyroscope)).not(),
        "The mobile phone might not have the correct capabilities for this!")(state,pp)
      Skip

    /** Gets the current orientation in degrees if available. (x,y,z) is also called (pitch, roll, yaw) or (alpha, beta, gamma). */
    // case "orientation" =>
    //   Top[S](TVector3.typ)
    // DECLARATION AS FIELD:
    //   /** Gets the current orientation in degrees if available. (x,y,z) is also called (pitch, roll, yaw) or (alpha, beta, gamma). */
    //   field_orientation = new TouchField("orientation",TVector3.typ)

    /** Records audio using the microphone */
    case "record_microphone" =>
      New[S](TSound.typ)

    /** Gets the gyroscope rotational velocity around each axis of the device, in degrees per second. */
    // case "rotation_speed" =>
    //   Top[S](TVector3.typ)
    // DECLARATION AS FIELD:
    //   /** Gets the gyroscope rotational velocity around each axis of the device, in degrees per second. */
    //   field_rotation_speed = new TouchField("rotation_speed",TVector3.typ)

    /** Takes a picture and returns it. This picture does not contain the gps location. */
    // case "take_camera_picture" =>
    //   Top[S](TPicture.typ)
    // DECLARATION AS FIELD:
    //   /** Takes a picture and returns it. This picture does not contain the gps location. */
    //   field_take_camera_picture = new TouchField("take_camera_picture",TPicture.typ)


    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }

}