package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State, VariableIdentifier}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 *
 * This is exactly the same as the GeoCoordinate class in C#
 * http://msdn.microsoft.com/en-us/library/system.device.location.geocoordinate.aspx
 *
 * Everything but lat/long is INVALID most of the time
 *
 *
 * User: Lucas Brutschy
 * Date: 11/8/12
 * Time: 6:10 PM
 */
object TLocation {


  /** Gets the latitude of the coordinate */
  val field_latitude = new TouchField("latitude", TNumber.typ) // -90 ndTo 90

  /** Gets the longitude of the coordinate */
  val field_longitude = new TouchField("longitude", TNumber.typ) // -180 ndTo 180

  /** Gets the altitude of the coordinate */
  val field_altitude = new TouchField("altitude", TNumber.typ)//, Invalid(TNumber.typ))

  /** Gets the speed of the coordinate */
  val field_speed = new TouchField("speed", TNumber.typ)//, Invalid(TNumber.typ))

  /** Gets the course of the coordinate, in degrees relative to true north */
  val field_course = new TouchField("course", TNumber.typ)//, Invalid(TNumber.typ)) // 0 ndTo 360

  /** Gets the horizontal accuracy of the coordinate */
  val field_hor_accuracy = new TouchField("hor_accuracy", TNumber.typ)//, Invalid(TNumber.typ))

  /** Gets the vertical accuracy of the coordinate */
  val field_vert_accuracy = new TouchField("vert_accuracy", TNumber.typ)//, Invalid(TNumber.typ))

  val typName = "Location"
  val typ = new TouchType(typName,isSingleton = false,List(field_latitude,field_longitude,field_altitude,
      field_speed, field_hor_accuracy,field_vert_accuracy))

}

class TLocation extends AAny {

  def getTyp = TLocation.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S = method match {

    /** Calculates the distance in meters */
    case "distance" =>
      val List(other) = parameters // Location
      New[S](TNumber.typ) // TODO

    /** Indicates if this instance is equal to the other */
    case "equals" =>
      val List(other) = parameters // Location
      New[S](TBoolean.typ) // TODO

    /** Displays the location in a map using Bing. */
    case "post_to_wall" =>
      Skip

    /** Shares the location (email, sms, facebook, social or '' to pick from a list) */
    case "share" =>
      val List(network,message) = parameters // String,String
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}

