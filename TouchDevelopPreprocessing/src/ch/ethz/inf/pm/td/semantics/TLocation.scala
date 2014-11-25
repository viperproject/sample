package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

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
object TLocation extends AAny {


  /** Gets the latitude of the coordinate */
  lazy val field_latitude = new ApiField("latitude", TNumber.typeName) // -90 ndTo 90

  /** Gets the longitude of the coordinate */
  lazy val field_longitude = new ApiField("longitude", TNumber.typeName) // -180 ndTo 180

  /** Gets the altitude of the coordinate */
  lazy val field_altitude = new ApiField("altitude", TNumber.typeName)//, Invalid(TNumber))

  /** Gets the speed of the coordinate */
  lazy val field_speed = new ApiField("speed", TNumber.typeName)//, Invalid(TNumber))

  /** Gets the course of the coordinate, in degrees relative to true north */
  lazy val field_course = new ApiField("course", TNumber.typeName)//, Invalid(TNumber)) // 0 ndTo 360

  /** Gets the horizontal accuracy of the coordinate */
  lazy val field_hor_accuracy = new ApiField("hor accuracy", TNumber.typeName)//, Invalid(TNumber))

  /** Gets the vertical accuracy of the coordinate */
  lazy val field_vert_accuracy = new ApiField("vert accuracy", TNumber.typeName)//, Invalid(TNumber))

  lazy val typeName = TypeName("Location")

  override def possibleFields = super.possibleFields ++ List(field_latitude,field_longitude,field_altitude,
      field_speed, field_hor_accuracy,field_course,field_vert_accuracy)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)(implicit pp:ProgramPoint,state:S):S = method match {

    /** Calculates the distance in meters */
    case "distance" =>
      val List(other) = parameters // Location
      Dummy[S](this0,method)
      Top[S](TNumber)

    /** Indicates if this instance is equal to the other */
    case "equals" =>
      val List(other) = parameters // Location
      Dummy[S](this0,method)
      Top[S](TBoolean)

    /** Displays the location in a map using Bing. */
    case "post to wall" =>
      Skip

    /** Shares the location (email, sms, facebook, social or '' to pick from a list) */
    case "share" =>
      val List(network,message) = parameters // String,String
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}

