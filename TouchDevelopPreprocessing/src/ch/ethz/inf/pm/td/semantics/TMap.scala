package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{VariableIdentifier, ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichExpression._

/**
 * User: lucas
 * Date: 11/22/12
 * Time: 6:34 PM
 */

object TMap {

  /** Gets the zoom level */
  val field_zoom = new TouchField("zoom",TNumber.typ)

  /** Gets the map center location */
  val field_center = new TouchField("center",TLocation.typ)

  val typName = "Map"
  val typ = new TouchType(typName,isSingleton = false,List(field_zoom,field_center))

}

class TMap extends AAny {

  def getTyp = TMap.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S = method match {

    /** Adds a polyline that passes through various geocoordinates */
    case "add_line" =>
      val List(locations,color,thickness) = parameters // Location_Collection,Color,Number
      Skip; // TODO

    /** Adds a link pushpin on the map (ignored if the location if not set) */
    case "add_link" =>
      val List(link,background,foreground) = parameters // Link,Color,Color
      Skip; // TODO

    /** Adds a message pushpin on the map (ignored if the location is not set) */
    case "add_message" =>
      val List(msg,background,foreground) = parameters // Message,Color,Color
      Skip; // TODO

    /** Adds a picture pushpin on the map */
    case "add_picture" =>
      val List(location,picture,background) = parameters // Location,Picture,Color
      Skip; // TODO

    /** Adds a place pushpin on the map (ignored if the location is not set) */
    case "add_place" =>
      val List(place,background,foreground) = parameters // Place,Color,Color
      Skip; // TODO

    /** Adds a text pushpin on the map */
    case "add_text" =>
      val List(location,text,background,foreground) = parameters // Location,String,Color,Color
      Skip; // TODO

    /** Clears the lines, regions and pushpins */
    case "clear" =>
      Skip; // TODO

    /** Fills a region with a color */
    case "fill_region" =>
      val List(locations,fill,stroke,thickness) = parameters // Location_Collection,Color,Color,Number
      Skip; // TODO

    /** Sets the map center location */
    case "set_center" =>
      val List(center) = parameters // Location
      Skip; // TODO

    /** Sets the zoom level from 1 (earth) to 21 (street) */
    case "set_zoom" =>
      val List(level) = parameters // Number
      CheckInRangeInclusive[S](level,1,21,method,"level")
      AssignField[S](this0,TMap.field_zoom,parameters.head)

    /** Changes the current zoom and center so that all the pushpins are visible. This method has no effect if the map is not posted on a the wall yet. */
    case "view_pushpins" =>
      // TODO: Check if already posted
      // PRECISION: We could implement this
      val state1 = AssignField(this0,TMap.field_zoom,1 ndTo 21)
      val state2 = Top[S](TLocation.typ)(state1,pp)
      AssignField(this0,TMap.field_center,state2.getExpression())(state2,pp)

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}

