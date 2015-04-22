
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Radio
 *
 * Access to the radio
 *
 * @author Lucas Brutschy
 */

trait Default_SRadio extends ASingleton {

  lazy val typeName = TypeName("Radio", isSingleton = true)
          
  /** Rarely used: Gets the frequency */
  def member_frequency = ApiMember(
    name = "frequency",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Indicates if the radio is on */
  def member_is_playing = ApiMember(
    name = "is playing",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: Creates a link to a radio frequency */
  def member_link_frequency = ApiMember(
    name = "link frequency",
    paramTypes = List(ApiParam(TString), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TLink,
    semantics = DefaultSemantics
  )

  /** Rarely used: Sets the frequency */
  def member_set_frequency = ApiMember(
    name = "set frequency",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the signal strength */
  def member_signal_strength = ApiMember(
    name = "signal strength",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Rarely used: Turns on the radio */
  def member_start = ApiMember(
    name = "start",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Turns off the radio */
  def member_stop = ApiMember(
    name = "stop",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "frequency" -> member_frequency,
    "is playing" -> member_is_playing,
    "link frequency" -> member_link_frequency,
    "set frequency" -> member_set_frequency,
    "signal strength" -> member_signal_strength,
    "start" -> member_start,
    "stop" -> member_stop
  )
            

}
          
