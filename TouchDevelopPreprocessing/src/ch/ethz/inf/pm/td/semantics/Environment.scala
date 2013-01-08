package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier

/**
 *
 * Lucas Brutschy
 * Date: 8/28/12
 * Time: 1:45 PM
 *
 */
object Environment {

  val leaderboardScore = new EnvironmentIdentifier("leaderboardScore",TNumber.typ,InitConstant("0"))
  val hasAccelerometer = new EnvironmentIdentifier("hasAccelerometer",TBoolean.typ,InitValid())
  val hasCompass = new EnvironmentIdentifier("hasCompass",TBoolean.typ,InitValid())
  val hasFrontCamera = new EnvironmentIdentifier("hasFrontCamera",TBoolean.typ,InitValid())
  val hasGyroscope = new EnvironmentIdentifier("hasGyroscope",TBoolean.typ,InitValid())
  val isLightTheme = new EnvironmentIdentifier("isLightTheme",TBoolean.typ,InitValid())
  val numberOfSongsInLibrary = new EnvironmentIdentifier("noSongsInLibrary",TNumber.typ,InitValid())
  val isConnected = new EnvironmentIdentifier("isConnected",TBoolean.typ,InitValid())

  val envs = List(leaderboardScore,hasAccelerometer,hasCompass,hasFrontCamera,hasGyroscope,isLightTheme,numberOfSongsInLibrary,isConnected)

}

/**
 * Our standard identifiers for the environment
 */
class EnvironmentIdentifier(name:String,typ:Type,val initializer:Initializer) extends VariableIdentifier(name,typ,null) {
  //override def getName() = name
  //override def getField() : Option[String] = None
  //override def representSingleVariable() = true
  override def toString() = "Env."+name
}

trait Initializer
case class InitTop() extends Initializer
case class InitValid() extends Initializer
case class InitConstant(s:String) extends Initializer




