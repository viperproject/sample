package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{TopWithInvalidInitializer, TouchField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of senses
 *
 * Camera, location, microphone and other sensors
 *
 * @author Lucas Brutschy
 */

object SSenses extends ASingleton {

  /** Gets the primary camera if available */
  lazy val field_camera = new TouchField("camera", TCamera.typeName, topDefault = TopWithInvalidInitializer("camera may not be available"))

  /** Gets the front facing camera if available */
  lazy val field_front_camera = new TouchField("front camera", TCamera.typeName, topDefault = TopWithInvalidInitializer("camera may not be available"))

  /** DEPRECATED. Test if the senses→acceleration quick is invalid instead */
  lazy val field_has_accelerometer = new TouchField("has accelerometer", TBoolean.typeName)

  /** DEPRECATED. Test if the senses→heading is invalid instead */
  lazy val field_has_compass = new TouchField("has compass", TBoolean.typeName)

  /** DEPRECATED. Test if the senses→front camera is invalid instead */
  lazy val field_has_front_camera = new TouchField("has front camera", TBoolean.typeName)

  /** Indicates if the gyroscope is available on the device */
  lazy val field_has_gyroscope = new TouchField("has gyroscope", TBoolean.typeName)

  /** Gets the charge level of the battery between 0 (discharged) and 1 (fully charged). Returns invalid if this information is not available. */
  lazy val field_battery_level = new TouchField("battery level", TNumber.typeName)

  /** Get the list of Bluetooth widgets paired with your device. */
  lazy val field_bluetooth_devices = new TouchField("bluetooth devices", GCollection(TBluetooth_Device.typeName).typeName)

  /** PRIVATE HANDLER FIELDS */
  lazy val field_shake_handler = new TouchField("shake handler", TAction.typeName)
  lazy val field_phone_face_up_handler = new TouchField("phone face up handler", TAction.typeName)
  lazy val field_phone_face_down_handler = new TouchField("phone face down handler", TAction.typeName)
  lazy val field_phone_portrait = new TouchField("phone portrait handler", TAction.typeName)
  lazy val field_phone_landscape_left = new TouchField("phone landscape left handler", TAction.typeName)
  lazy val field_phone_langscape_right = new TouchField("phone landscape right handler", TAction.typeName)

  lazy val typeName = TypeName("Senses")

  override def possibleFields = super.possibleFields ++
    List(field_front_camera, field_camera, field_has_accelerometer, field_has_compass, field_has_front_camera,
      field_has_gyroscope, field_bluetooth_devices, field_battery_level, field_shake_handler, field_phone_face_up_handler,
      field_phone_face_down_handler, field_phone_portrait, field_phone_landscape_left, field_phone_langscape_right
    )

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Gets filtered accelerometer data using a combination of a low-pass and threshold triggered high-pass on each axis to eliminate the majority of the sensor low amplitude noise while trending very quickly to large offsets (not perfectly smooth signal in that case), providing a very low latency. This is ideal for quickly reacting UI updates. */
    case "acceleration quick" =>
      val res = If[S](Field[S](this0, SSenses.field_has_accelerometer),
        Then = {
          s: S =>
            Top[S](TVector3)(s, pp)
        }, Else = {
          s: S =>
            Return[S](Invalid(TVector3, "accelerometer may be unavailable on the users device"))(s, pp)
        }
      )
      res

    /** Gets filtered accelerometer data using a 1 Hz first-order low-pass on each axis to eliminate the main sensor noise while providing a medium latency. This can be used for moderately reacting UI updates requiring a very smooth signal. */
    case "acceleration smooth" =>
      If[S](Field[S](this0, SSenses.field_has_accelerometer),
        Then = {
          s: S =>
            Top[S](TVector3)(s, pp)
        }, Else = {
          s: S =>
            Return[S](Invalid(TVector3, "accelerometer may be unavailable on the users device"))(s, pp)
        }
      )

    /** Gets filtered and temporally averaged accelerometer data using an arithmetic mean of the last 25 'optimally filtered' samples, so over 500ms at 50Hz on each axis, to virtually eliminate most sensor noise. This provides a very stable reading but it has also a very high latency and cannot be used for rapidly reacting UI. */
    case "acceleration stable" =>
      If[S](Field[S](this0, SSenses.field_has_accelerometer),
        Then = {
          s: S =>
            Top[S](TVector3)(s, pp)
        }, Else = {
          s: S =>
            Return[S](Invalid(TVector3, "accelerometer may be unavailable on the users device"))(s, pp)
        }
      )

    /** Gets the current phone location. The phone optimizes the accuracy for power, performance, and other cost considerations. */
    case "current location" =>
      TopWithInvalid[S](TLocation, "gps sensor may be unavailable")

    /** Gets the current phone location with the most accuracy. This includes using services that might charge money,
      * or consuming higher levels of battery power or connection bandwidth. */
    case "current location accurate" =>
      TopWithInvalid[S](TLocation, "gps sensor may be unavailable")

    /** DEPRECATED. Test if the senses→motion is invalid instead. */
    case "has motion" =>
      Return[S](Field[S](this0, SSenses.field_has_accelerometer)
        && Field[S](this0, SSenses.field_has_compass)
        && Field[S](this0, SSenses.field_has_gyroscope))

    /** Gets the compass heading, in degrees, measured clockwise from the Earth's geographic north. */
    case "heading" =>
      If[S](Field[S](this0, SSenses.field_has_compass),
        Then = {
          s: S =>
            Top[S](TNumber)(s, pp)
        }, Else = {
          s: S =>
            Return[S](Invalid(TNumber, "compass may be unavailable on the users device"))(s, pp)
        }
      )

    /** Indicates whether the device is 'stable' (no movement for about 0.5 seconds) */
    case "is device stable" =>
      If[S](Field[S](this0, SSenses.field_has_accelerometer), Then = {
        s: S =>
          Top[S](TBoolean)(s, pp)
      }, Else = {
        s: S =>
          Return[S](Invalid(TBoolean, "motion sensor may be unavailable on the users device"))(s, pp)
      })

    /** Gets the current motion that combines data from the accelerometer, compass and gyroscope if available. */
    case "motion" =>
      val res = If[S](Field[S](this0, SSenses.field_has_accelerometer)
        && Field[S](this0, SSenses.field_has_compass)
        && Field[S](this0, SSenses.field_has_gyroscope),
        Then = {
          s: S =>
            Top[S](TMotion)(s, pp)
        }, Else = {
          s: S =>
            Return[S](Invalid(TMotion, "motion sensor may be unavailable on the users device"))(s, pp)
        }
      )
      res

    /** Attaches an event that triggers while the key is pressed. This event repeats while the key is down. */
    case "on key pressed" =>
      val List(key, handler) = parameters // String,Action
      New[S](TEvent_Binding)

    /** Attaches a handler to the `phone face down` event. */
    case "on phone face down" =>
      val List(handler) = parameters // Action
    val newState = AssignField[S](this0, SSenses.field_phone_face_down_handler, handler)
      New[S](TEvent_Binding)(newState, pp)

    /** Attaches a handler to the `phone face up` event. */
    case "on phone face up" =>
      val List(handler) = parameters // Action
    val newState = AssignField[S](this0, SSenses.field_phone_face_up_handler, handler)
      New[S](TEvent_Binding)(newState, pp)

    /** Attaches a handler to the `phone landscape left` event. */
    case "on phone landscape left" =>
      val List(handler) = parameters // Action
    val newState = AssignField[S](this0, SSenses.field_phone_landscape_left, handler)
      New[S](TEvent_Binding)(newState, pp)

    /** Attaches a handler to the `phone landscape right` event. */
    case "on phone landscape right" =>
      val List(handler) = parameters // Action
    val newState = AssignField[S](this0, SSenses.field_phone_langscape_right, handler)
      New[S](TEvent_Binding)(newState, pp)

    /** Attaches a handler to the `phone portrait` event. */
    case "on phone portrait" =>
      val List(handler) = parameters // Action
    val newState = AssignField[S](this0, SSenses.field_phone_portrait, handler)
      New[S](TEvent_Binding)(newState, pp)

    /** Attaches a handler to the `shake` event. */
    case "on shake" =>
      val List(handler) = parameters // Action
    val newState = AssignField[S](this0, SSenses.field_shake_handler, handler)
      New[S](TEvent_Binding)(newState, pp)

    /** Gets the current orientation in degrees if available. (x,y,z) is also called (pitch, roll, yaw) or (alpha, beta, gamma). */
    case "orientation" =>
      If[S](Field[S](this0, SSenses.field_has_gyroscope), Then = {
        s: S =>
          Top[S](TVector3)(s, pp)
      }, Else = {
        s: S =>
          Return[S](Invalid(TVector3, "gyroscope sensor may be unavailable"))(s, pp)
      })

    /** Records audio using the microphone */
    case "record microphone" =>
      TopWithInvalid[S](TSound, "microphone may be unavailable")

    /** Gets the gyroscope rotational velocity around each axis of the device, in degrees per second. */
    case "rotation speed" =>
      If[S](Field[S](this0, SSenses.field_has_gyroscope), Then = {
        s: S =>
          Top[S](TVector3)(s, pp)
      }, Else = {
        s: S =>
          Return[S](Invalid(TVector3, "gyroscope may be unavailable on the users device"))(s, pp)
      })

    /** Takes a picture and returns it. This picture does not contain the gps location. */
    case "take camera picture" =>
      If[S](Field[S](this0, SSenses.field_camera) equal Invalid(TCamera, "camera may be unavailable on the users device"),
        Then = {
          Return[S](Invalid(TPicture, "camera may be unavailable on the users device"))(_, pp)
        },
        Else = {
          Top[S](TPicture, Map(
            TPicture.field_location -> Invalid(TLocation, "gps sensor may be unavailable on the users device")
          ))(_, pp)
        }
      )


    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }

}