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

  /** Gets the primary camera if available */
  val field_camera = new TouchField("camera",TCamera.typ,TopWithInvalidInitializer())

  /** Gets the front facing camera if available */
  val field_front_camera = new TouchField("front camera",TCamera.typ,TopWithInvalidInitializer())

  /** DEPRECATED. Test if the senses→acceleration quick is invalid instead */
  val field_has_accelerometer = new TouchField("has accelerometer",TBoolean.typ)

  /** DEPRECATED. Test if the senses→heading is invalid instead */
  val field_has_compass = new TouchField("has compass",TBoolean.typ)

  /** DEPRECATED. Test if the senses→front camera is invalid instead */
  val field_has_front_camera = new TouchField("has front camera",TBoolean.typ)

  /** Indicates if the gyroscope is available on the device */
  val field_has_gyroscope = new TouchField("has gyroscope",TBoolean.typ)

  val typName = "Senses"
  val typ = new TouchType(typName,isSingleton = true,
    fields = List(field_front_camera,field_camera,field_has_accelerometer,field_has_compass,field_has_front_camera,field_has_gyroscope))

}

class SSenses extends AAny {

  def getTyp = SSenses.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    /** Gets filtered accelerometer data using a combination of a low-pass and threshold triggered high-pass on each axis to eliminate the majority of the sensor low amplitude noise while trending very quickly to large offsets (not perfectly smooth signal in that case), providing a very low latency. This is ideal for quickly reacting UI updates. */
    case "acceleration quick" =>
      If[S](Field[S](this0,SSenses.field_has_accelerometer),
        Then = { s:S =>
          Top[S](TVector3.typ)(s,pp)
        }, Else = { s:S =>
          Return[S](Invalid(TVector3.typ))(s,pp)
        }
      )

    /** Gets filtered accelerometer data using a 1 Hz first-order low-pass on each axis to eliminate the main sensor noise while providing a medium latency. This can be used for moderately reacting UI updates requiring a very smooth signal. */
    case "acceleration smooth" =>
      If[S](Field[S](this0,SSenses.field_has_accelerometer),
        Then = { s:S =>
          Top[S](TVector3.typ)(s,pp)
        }, Else = { s:S =>
          Return[S](Invalid(TVector3.typ))(s,pp)
        }
      )

    /** Gets filtered and temporally averaged accelerometer data using an arithmetic mean of the last 25 'optimally filtered' samples, so over 500ms at 50Hz on each axis, to virtually eliminate most sensor noise. This provides a very stable reading but it has also a very high latency and cannot be used for rapidly reacting UI. */
    case "acceleration stable" =>
      If[S](Field[S](this0,SSenses.field_has_accelerometer),
        Then = { s:S =>
          Top[S](TVector3.typ)(s,pp)
        }, Else = { s:S =>
          Return[S](Invalid(TVector3.typ))(s,pp)
        }
      )

    /** Gets the current phone location. The phone optimizes the accuracy for power, performance, and other cost considerations. */
    case "current location" =>
       TopWithInvalid[S](TLocation.typ)

    /** Gets the current phone location with the most accuracy. This includes using services that might charge money,
      * or consuming higher levels of battery power or connection bandwidth. */
     case "current location accurate" =>
       TopWithInvalid[S](TLocation.typ)

    /** DEPRECATED. Test if the senses→motion is invalid instead. */
    case "has motion" =>
      Return[S](Field[S](this0,SSenses.field_has_accelerometer)
        && Field[S](this0,SSenses.field_has_compass)
        && Field[S](this0,SSenses.field_has_gyroscope))

    /** Gets the compass heading, in degrees, measured clockwise from the Earth's geographic north. */
     case "heading" =>
       If[S](Field[S](this0,SSenses.field_has_compass),
         Then = { s:S =>
           Top[S](TNumber.typ)(s,pp)
         }, Else = { s:S =>
           Return[S](Invalid(TNumber.typ))(s,pp)
         }
       )

    /** Indicates whether the device is 'stable' (no movement for about 0.5 seconds) */
    case "is device stable" =>
      If[S](Field[S](this0,SSenses.field_has_accelerometer),Then = { s:S =>
        Top[S](TBoolean.typ)(s,pp)
      }, Else = { s:S =>
        Return[S](Invalid(TBoolean.typ))(s,pp)
      })

    /** Gets the current motion that combines data from the accelerometer, compass and gyroscope if available. */
    case "motion" =>
      If[S](Field[S](this0,SSenses.field_has_accelerometer)
        && Field[S](this0,SSenses.field_has_compass)
        && Field[S](this0,SSenses.field_has_gyroscope),
        Then = { s:S =>
          Top[S](TMotion.typ)(s,pp)
        }, Else = { s:S =>
          Return[S](Invalid(TMotion.typ))(s,pp)
        }
      )


    /** Gets the current orientation in degrees if available. (x,y,z) is also called (pitch, roll, yaw) or (alpha, beta, gamma). */
    case "orientation" =>
      If[S](Field[S](this0,SSenses.field_has_gyroscope),Then = { s:S =>
        Top[S](TVector3.typ)(s,pp)
      }, Else = { s:S =>
        TopWithInvalid[S](TVector3.typ)(s,pp)
      })

    /** Records audio using the microphone */
    case "record microphone" =>
      New[S](TSound.typ)

    /** Gets the gyroscope rotational velocity around each axis of the device, in degrees per second. */
    case "rotation speed" =>
      If[S](Field[S](this0,SSenses.field_has_gyroscope),Then = { s:S =>
        Top[S](TVector3.typ)(s,pp)
      }, Else = { s:S =>
        Return[S](Invalid(TVector3.typ))(s,pp)
      })

    /** Takes a picture and returns it. This picture does not contain the gps location. */
    case "take camera picture" =>
      If[S](Field[S](this0,SSenses.field_camera) equal Invalid(TCamera.typ),
        Then = {
          Return[S](Invalid(TPicture.typ))(_,pp)
        },
        Else = {
          Top[S](TPicture.typ,Map(
            TPicture.field_location -> Invalid(TLocation.typ)
          ))(_,pp)
        }
      )


    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }

}