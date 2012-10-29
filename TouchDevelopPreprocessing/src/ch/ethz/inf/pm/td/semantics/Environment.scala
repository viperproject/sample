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

  val hasAccelerometer = new EnvironmentIdentifier("Accelerometer",Boolean)
  val hasCompass = new EnvironmentIdentifier("Compass",Boolean)
  val hasFrontCamera = new EnvironmentIdentifier("FrontCamera",Boolean)
  val hasGyroscope = new EnvironmentIdentifier("Gyroscope",Boolean)

  val envs = List(hasAccelerometer,hasCompass,hasFrontCamera,hasGyroscope)


}
