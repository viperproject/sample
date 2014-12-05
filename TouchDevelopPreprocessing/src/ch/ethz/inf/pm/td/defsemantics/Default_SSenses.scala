
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Senses
 *
 * Camera, location, microphone and other sensors
 *
 * @author Lucas Brutschy
 */

trait Default_SSenses extends ASingleton {

  lazy val typeName = TypeName("Senses")
          
  /** Sometimes used: Gets filtered accelerometer data using a combination of a low-pass and threshold triggered high-pass on each axis to eliminate the majority of the sensor low amplitude noise while trending very quickly to large offsets (not perfectly smooth signal in that case), providing a very low latency. This is ideal for quickly reacting UI updates. */
  def member_acceleration_quick = ApiMember(
    name = "acceleration quick",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TVector3,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets filtered accelerometer data using a 1 Hz first-order low-pass on each axis to eliminate the main sensor noise while providing a medium latency. This can be used for moderately reacting UI updates requiring a very smooth signal. */
  def member_acceleration_smooth = ApiMember(
    name = "acceleration smooth",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TVector3,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets filtered and temporally averaged accelerometer data using an arithmetic mean of the last 25 'optimally filtered' samples, so over 500ms at 50Hz on each axis, to virtually eliminate most sensor noise. This provides a very stable reading but it has also a very high latency and cannot be used for rapidly reacting UI. */
  def member_acceleration_stable = ApiMember(
    name = "acceleration stable",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TVector3,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the charge level of the battery between 0 (discharged) and 1 (fully charged). Returns invalid if this information is not available. */
  def member_battery_level = ApiMember(
    name = "battery level",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: [**dbg**] Get the list of Bluetooth LE devices paired with your device. */
  def member_bluetoothLE_devices = ApiMember(
    name = "bluetooth LE devices",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TBluetooth_Le_Device),
    semantics = DefaultSemantics
  )

  /** Never used: Get the list of Bluetooth widgets paired with your device. */
  def member_bluetooth_devices = ApiMember(
    name = "bluetooth devices",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TBluetooth_Device),
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the primary camera if available */
  def member_camera = ApiMember(
    name = "camera",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TCamera,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the current phone location with the most accuracy. This includes using services that might charge money, or consuming higher levels of battery power or connection bandwidth. */
  def member_current_location_accurate = ApiMember(
    name = "current location accurate",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TLocation,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the current approximate phone location. The phone optimizes the accuracy for power, performance, and other cost considerations. */
  def member_current_location = ApiMember(
    name = "current location",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TLocation,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the first connected gamepad available */
  def member_first_gamepad = ApiMember(
    name = "first gamepad",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TGamepad,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the front facing camera if available */
  def member_front_camera = ApiMember(
    name = "front camera",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TCamera,
    semantics = DefaultSemantics
  )

  /** Never used: Gets a snapshot of the gamepad states (if any connected to the browser). Empty if unsupported or no gamepad connected. */
  def member_gamepads = ApiMember(
    name = "gamepads",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TGamepad),
    semantics = DefaultSemantics
  )

  /** Rarely used: Indicates if an accelerometer is available. */
  def member_has_accelerometer = ApiMember(
    name = "has accelerometer",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**obsolete**] Test if the senses→heading is invalid instead */
  def member_has_compass = ApiMember(
    name = "has compass",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**obsolete**] Test if the senses→front camera is invalid instead */
  def member_has_front_camera = ApiMember(
    name = "has front camera",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Rarely used: Indicates if the gyroscope is available on the device */
  def member_has_gyroscope = ApiMember(
    name = "has gyroscope",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] [**obsolete**] Test if the senses→motion is invalid instead. */
  def member_has_motion = ApiMember(
    name = "has motion",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the compass heading, in degrees, measured clockwise from the Earth’s geographic north. */
  def member_heading = ApiMember(
    name = "heading",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Rarely used: Indicates whether the device is 'stable' (no movement for about 0.5 seconds) */
  def member_is_device_stable = ApiMember(
    name = "is device stable",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: Indicates if the specified key is pressed. */
  def member_is_key_pressed = ApiMember(
    name = "is key pressed",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Sometimes used: [**not implemented**] Gets the current motion that combines data from the accelerometer, compass and gyroscope if available. */
  def member_motion = ApiMember(
    name = "motion",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TMotion,
    semantics = DefaultSemantics
  )

  /** Never used: Attaches an event that triggers while the key is pressed. This event repeats while the key is down. */
  def member_on_key_pressed = ApiMember(
    name = "on key pressed",
    paramTypes = List(ApiParam(TString), ApiParam(TAction)),
    thisType = ApiParam(this),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Never used: Attaches a handler to the `phone face down` event. */
  def member_on_phone_face_down = ApiMember(
    name = "on phone face down",
    paramTypes = List(ApiParam(TAction)),
    thisType = ApiParam(this),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Never used: Attaches a handler to the `phone face up` event. */
  def member_on_phone_face_up = ApiMember(
    name = "on phone face up",
    paramTypes = List(ApiParam(TAction)),
    thisType = ApiParam(this),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Never used: Attaches a handler to the `phone landscape left` event. */
  def member_on_phone_landscape_left = ApiMember(
    name = "on phone landscape left",
    paramTypes = List(ApiParam(TAction)),
    thisType = ApiParam(this),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Never used: Attaches a handler to the `phone landscape right` event. */
  def member_on_phone_landscape_right = ApiMember(
    name = "on phone landscape right",
    paramTypes = List(ApiParam(TAction)),
    thisType = ApiParam(this),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Never used: Attaches a handler to the `phone portrait` event. */
  def member_on_phone_portrait = ApiMember(
    name = "on phone portrait",
    paramTypes = List(ApiParam(TAction)),
    thisType = ApiParam(this),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Never used: Attaches a handler to the `shake` event. */
  def member_on_shake = ApiMember(
    name = "on shake",
    paramTypes = List(ApiParam(TAction)),
    thisType = ApiParam(this),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the current orientation in degrees if available. (x,y,z) is also called (pitch, roll, yaw) or (alpha, beta, gamma). */
  def member_orientation = ApiMember(
    name = "orientation",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TVector3,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Records audio using the microphone */
  def member_record_microphone = ApiMember(
    name = "record microphone",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TSound,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the gyroscope rotational velocity around each axis of the device, in degrees per second. */
  def member_rotation_speed = ApiMember(
    name = "rotation speed",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TVector3,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Takes a picture and returns it. This picture does not contain the gps location. */
  def member_take_camera_picture = ApiMember(
    name = "take camera picture",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPicture,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "acceleration quick" -> member_acceleration_quick,
    "acceleration smooth" -> member_acceleration_smooth,
    "acceleration stable" -> member_acceleration_stable,
    "battery level" -> member_battery_level,
    "bluetooth LE devices" -> member_bluetoothLE_devices,
    "bluetooth devices" -> member_bluetooth_devices,
    "camera" -> member_camera,
    "current location accurate" -> member_current_location_accurate,
    "current location" -> member_current_location,
    "first gamepad" -> member_first_gamepad,
    "front camera" -> member_front_camera,
    "gamepads" -> member_gamepads,
    "has accelerometer" -> member_has_accelerometer,
    "has compass" -> member_has_compass,
    "has front camera" -> member_has_front_camera,
    "has gyroscope" -> member_has_gyroscope,
    "has motion" -> member_has_motion,
    "heading" -> member_heading,
    "is device stable" -> member_is_device_stable,
    "is key pressed" -> member_is_key_pressed,
    "motion" -> member_motion,
    "on key pressed" -> member_on_key_pressed,
    "on phone face down" -> member_on_phone_face_down,
    "on phone face up" -> member_on_phone_face_up,
    "on phone landscape left" -> member_on_phone_landscape_left,
    "on phone landscape right" -> member_on_phone_landscape_right,
    "on phone portrait" -> member_on_phone_portrait,
    "on shake" -> member_on_shake,
    "orientation" -> member_orientation,
    "record microphone" -> member_record_microphone,
    "rotation speed" -> member_rotation_speed,
    "take camera picture" -> member_take_camera_picture
  )
            

}
          
