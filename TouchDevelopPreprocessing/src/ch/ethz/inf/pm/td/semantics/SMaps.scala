package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{State, ExpressionSet}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.td.analysis.TouchAnalysisParameters
import ch.ethz.inf.pm.td.analysis.interpreter.{InvalidV, ConcreteInterpreter, TouchValue}

/**
 * User: lucas
 * Date: 11/26/12
 * Time: 1:09 PM
 */

object SMaps {

  val typName = "Maps"
  val typ = DefaultTouchType(typName, isSingleton = true)

}

class SMaps extends AAny {

  def getTyp = SMaps.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Creates a full screen Bing map. Use 'post to wall' to display it. */
    case "create full map" =>
      New[S](TMap.typ) // PRECISION: We might want to set zoom/center here

    /** Creates a Bing map. Use 'post to wall' to display it. */
    case "create map" =>
      New[S](TMap.typ) // PRECISION: We might want to set zoom/center here

    /** Calculates the directions between two coordinates using Bing. */
    case "directions" =>
      val List(from,to,walking) = parameters // Location,Location,Boolean
      if (TouchAnalysisParameters.reportPrematurelyOnInternetAccess)
        Error[S](Field[S](Singleton(SWeb.typ),SWeb.field_is_connected).not(),"directions",
          "Check if the device is connected to the internet before using the connection")
      // may return invalid
      NonDetReturn[S](TLocation_Collection.typ)

    /** Shows the directions in the Bing map application. If search term is provided, location is ignored.
        Provide search term or location for start and end. */
    case "open directions" =>
      val List(start_search,start_loc,end_search,end_loc) = parameters // String,Location,String,Location
      Error[S](Field[S](Singleton(SWeb.typ),SWeb.field_is_connected).not(),"open directions",
        "Check if the device is connected to the internet before opening the directions")
      Skip

    /** Opens the Bing map application. zoom between 0 (close) and 1 (far). */
    case "open map" =>
      val List(center,search,zoom) = parameters // Location,String,Number
      Error[S](Field[S](Singleton(SWeb.typ),SWeb.field_is_connected).not(),"open map",
        "Check if the device is connected to the internet before opening a map")
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](zoom,0,1,method,"zoom")
      }
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }

  override def concreteSemantics(this0: TouchValue,
                                 method: String,
                                 params: List[TouchValue],
                                 interpreter: ConcreteInterpreter,
                                 pp: ProgramPoint): TouchValue = method match {

    case "directions" =>
      val List(fromLocation, toLocation, _) = params
      interpreter
        .nonDetInputAt(pp)
        .getOrElse(InvalidV(TLocation_Collection.typ))

    case "create map" =>
      interpreter.state.createObject(TMap.typ)

    case _ => super.concreteSemantics(this0, method, params, interpreter, pp)

  }
}
