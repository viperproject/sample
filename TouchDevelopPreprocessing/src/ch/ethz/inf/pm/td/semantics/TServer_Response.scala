
/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TServer_Response
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Server Response
 *
 * An HTTP web response to be returned
 *
 * @author Lucas Brutschy
 */ 

object TServer_Response extends Default_TServer_Response {

  /** [**beta**] Gets the HTTP Status code of the response (defaults to 200) */
  lazy val field_status_code = ApiField("status code", TNumber)

  /** [**beta**] Gets the names of the headers */
  lazy val field_header_map = ApiField("header map", TString_Map)

  /** [**beta**] Gets the request associated to this response */
  lazy val field_request = ApiField("request", TServer_Request)

  override def possibleFields = super.possibleFields ++ List(field_header_map, field_request, field_status_code)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** [**beta**] Indicates if both responses are the same instance. */
    case "equals" =>
       //val List(other) = parameters // Server_Response
       Top[S](TBoolean)

    /** [**beta**] Gets the value of a given header */
    case "header" =>
      val List(name) = parameters // String
      CallApi[S](Field[S](this0,TServer_Response.field_header_map),"at",List(name),returnedType)    // DECLARATION AS FIELD:

    /** [**beta**] Sets the content of the response as a binary buffer */
    case "set content as buffer" =>
       val List(bytes) = parameters // Buffer
       Skip

    /** [**beta**] Sets the content of the response as the JSON tree */
    case "set content as json" =>
       val List(json) = parameters // Json_Object
       Skip

    /** [**beta**] Sets the content of the response as the XML tree */
    case "set content as xml" =>
       val List(xml) = parameters // Xml_Object
       Skip

    /** [**beta**] Sets the 'Content-Type' HTTP header; call after `->set_content...` */
    case "set content type" =>
       val List(typ) = parameters // String
      CallApi[S](Field[S](this0,TServer_Response.field_header_map),"set at",List(String("Content-Type"),typ),returnedType)

    /** [**beta**] Sets the content of the response */
    case "set content" =>
       val List(content) = parameters // String
       Skip

    /** [**beta**] Sets an HTTP header value. Empty string clears the value */
    case "set header" =>
       val List(name,value) = parameters // String,String
       CallApi[S](Field[S](this0,TServer_Response.field_header_map),"set at",List(name,value),returnedType)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
