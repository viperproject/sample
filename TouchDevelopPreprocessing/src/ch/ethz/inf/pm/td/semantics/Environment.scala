package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.domain.EnvironmentIdentifier
import ch.ethz.inf.pm.td.compiler.TouchType

/**
 *
 * Lucas Brutschy
 * Date: 8/28/12
 * Time: 1:45 PM
 *
 */

// THIS DOESNT REALLY BELONG HERE
object Environment {

  val Boolean = TouchType("Boolean")
  val Number = TouchType("Number")

  val hasAccelerometer = new EnvironmentIdentifier("hasAccelerometer",Boolean)
  val hasCompass = new EnvironmentIdentifier("hasCompass",Boolean)
  val hasFrontCamera = new EnvironmentIdentifier("hasFrontCamera",Boolean)
  val hasGyroscope = new EnvironmentIdentifier("hasGyroscope",Boolean)
  val isLightTheme = new EnvironmentIdentifier("isLightTheme",Boolean)
  val noSongsInLibrary = new EnvironmentIdentifier("noSongsInLibrary",Number)

  val envs = List(hasAccelerometer,hasCompass,hasFrontCamera,hasGyroscope,isLightTheme,noSongsInLibrary)


}
