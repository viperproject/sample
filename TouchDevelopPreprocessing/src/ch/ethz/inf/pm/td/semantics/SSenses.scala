package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * User: lucas
 * Date: 11/8/12
 * Time: 6:53 PM
 */
class SSenses extends Any {

  def getTypeName = "senses"

  def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S = method match {

    case "has_accelerometer" => Return[S](Environment.hasAccelerometer)
    case "has_compass" => Return[S](Environment.hasCompass)
    case "has_front_camera" => Return[S](Environment.hasFrontCamera)
    case "has_gyroscope" => Return[S](Environment.hasGyroscope)
    case "has_motion" => Return[S](RichExpression(Environment.hasAccelerometer) && RichExpression(Environment.hasCompass) && RichExpression(Environment.hasGyroscope))

    case "motion" =>
      Error((RichExpression(Environment.hasAccelerometer)
        && RichExpression(Environment.hasCompass)
        && RichExpression(Environment.hasGyroscope)).not(),
        "The mobile phone might not have the correct capabilities for this!")(state,pp)
      Skip

    case _ =>
      Unimplemented[S](this0.getType().toString+"."+method)

  }

}