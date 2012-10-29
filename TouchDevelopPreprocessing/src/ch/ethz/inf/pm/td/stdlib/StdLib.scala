package ch.ethz.inf.pm.td.stdlib

import ch.ethz.inf.pm.td.symbols.{AbstractSymbolTable, Member}

/**
 *
 * Lucas Brutschy
 * Date: 8/9/12
 * Time: 6:13 PM
 *
 */

trait StdLib extends AbstractSymbolTable  {

  addSingleton("bazaar", GenericTypes.gAlsoSingletons("bazaar") :::List(	// Browse and review scripts from the bazaar
    Member("leaderboard_score","Number"),	//Gets the current score for the current script
    Member("post_leaderboard_score",List("Number"),"Nothing"),	//Posts the current game score to the script leaderboard
    Member("post_leaderboard_to_wall","Nothing")	//Posts the current game leaderboard to the wall
  ))
  addSingleton("collections", GenericTypes.gAlsoSingletons("collections") :::List(	// Create collections of items.
    Member("create_link_collection","Link_Collection"),	//Creates an empty link collection
    Member("create_location_collection","Location_Collection"),	//Creates an empty location collection
    Member("create_message_collection","Message_Collection"),	//Creates an empty message collection
    Member("create_number_collection","Number_Collection"),	//Creates an empty number collection
    Member("create_number_map","Number_Map"),	//Creates an empty number map
    Member("create_place_collection","Place_Collection"),	//Creates an empty place collection
    Member("create_string_collection","String_Collection"),	//Creates an empty string collection
    Member("create_string_map","String_Map")	//Creates an empty string map (case and culture sensitive)
  ))
  addSingleton("colors", GenericTypes.gAlsoSingletons("colors") :::List(	// New or built-in colors
    Member("accent","Color"),	//Gets the accent color in the current theme
    Member("background","Color"),	//Gets the background color in the current theme
    Member("black","Color"),	//Gets the color that has the ARGB value of #FF000000
    Member("blue","Color"),	//Gets the color that has the ARGB value of #FF0000FF
    Member("brown","Color"),	//Gets the color that has the ARGB value of #FFA52A2A
    Member("chrome","Color"),	//Gets the chrome color in the current theme (control background)
    Member("cyan","Color"),	//Gets the color that has the ARGB value of #FF00FFFF
    Member("dark_gray","Color"),	//Gets the color that has the ARGB value of #FFA9A9A9
    Member("foreground","Color"),	//Gets the foreground color in the current theme
    Member("from_ahsb",List("Number","Number","Number","Number"),"Color"),	//Creates a color from the alpha, hue, saturation, brightness channels (0.0-1.0 range)
    Member("from_argb",List("Number","Number","Number","Number"),"Color"),	//Creates a color from the alpha, red, green, blue channels (0.0-1.0 range)
    Member("from_hsb",List("Number","Number","Number"),"Color"),	//Creates a color from the hue, saturation, brightness channels (0.0-1.0 range)
    Member("from_rgb",List("Number","Number","Number"),"Color"),	//Creates a color from the red, green, blue channels (0.0-1.0 range)
    Member("gray","Color"),	//Gets the color that has the ARGB value of #FF808080
    Member("green","Color"),	//Gets the color that has the ARGB value of #FF008000
    Member("is_light_theme","Boolean"),	//Indicates if the user is using a light theme in his phone
    Member("light_gray","Color"),	//Gets the color that has the ARGB value of #FFD3D3D3
    Member("linear_gradient",List("Color","Color","Number"),"Color"),	//Computes an intermediate color
    Member("magenta","Color"),	//Gets the color that has the ARGB value of #FFFF00FF
    Member("orange","Color"),	//Gets the color that has the ARGB value of #FFFFA500
    Member("purple","Color"),	//Gets the color that has the ARGB value of #FF800080
    Member("random","Color"),	//Picks a random color
    Member("red","Color"),	//Gets the color that has the ARGB value of #FFFF0000
    Member("sepia","Color"),	//Gets the color that has the ARGB value of #FF704214
    Member("subtle","Color"),	//Gets the subtle color in the current theme (light gray)
    Member("transparent","Color"),	//Gets the color that has the ARGB value of #00FFFFFF
    Member("white","Color"),	//Gets the color that has the ARGB value of #FFFFFFFF
    Member("yellow","Color")	//Gets the color that has the ARGB value of #FFFFFF00
  ))
  addSingleton("data", GenericTypes.gAlsoSingletons("data") :::List(
  ))
  addSingleton("home", GenericTypes.gAlsoSingletons("home") :::List(	// Interact with devices in the home network. Devices must be UPnPâ„¢ compatible.
    Member("choose_player","Media_Player"),	//Choose a media player on the current wireless network
    Member("choose_printer","Printer"),	//Choose a printer on the current wireless network
    Member("choose_server","Media_Server"),	//Choose a media server on the current wireless network
    Member("players","Media_Player_Collection"),	//Gets the media players on the current wireless network
    Member("printers","Printer_Collection"),	//Gets the printers on the current wireless network
    Member("servers","Media_Server_Collection")	//Gets the media servers on the home network
  ))
  addSingleton("invalid", GenericTypes.gAlsoSingletons("invalid") :::List(	// Create invalid values
    Member("appointment","Appointment"),	//Creates an invalid Appointment instance
    Member("appointment_collection","Appointment_Collection"),	//Creates an invalid Appointment Collection instance
    Member("board","Board"),	//Creates an invalid Board instance
    Member("boolean","Boolean"),	//Creates an invalid Boolean instance
    Member("camera","Camera"),	//Creates an invalid Camera instance
    Member("color","Color"),	//Creates an invalid Color instance
    Member("contact","Contact"),	//Creates an invalid Contact instance
    Member("contact_collection","Contact_Collection"),	//Creates an invalid Contact Collection instance
    Member("datetime","DateTime"),	//Creates an invalid DateTime instance
    Member("device","Device"),	//Creates an invalid Device instance
    Member("device_collection","Device_Collection"),	//Creates an invalid Device Collection instance
    Member("json_object","Json_Object"),	//Creates an invalid Json Object instance
    Member("link","Link"),	//Creates an invalid Link instance
    Member("link_collection","Link_Collection"),	//Creates an invalid Link Collection instance
    Member("location","Location"),	//Creates an invalid Location instance
    Member("location_collection","Location_Collection"),	//Creates an invalid Location Collection instance
    Member("map","Map"),	//Creates an invalid Map instance
    Member("media_link","Media_Link"),	//Creates an invalid Media Link instance
    Member("media_link_collection","Media_Link_Collection"),	//Creates an invalid Media Link Collection instance
    Member("media_player","Media_Player"),	//Creates an invalid Media Player instance
    Member("media_player_collection","Media_Player_Collection"),	//Creates an invalid Media Player Collection instance
    Member("media_server","Media_Server"),	//Creates an invalid Media Server instance
    Member("media_server_collection","Media_Server_Collection"),	//Creates an invalid Media Server Collection instance
    Member("message","Message"),	//Creates an invalid Message instance
    Member("message_collection","Message_Collection"),	//Creates an invalid Message Collection instance
    Member("motion","Motion"),	//Creates an invalid Motion instance
    Member("number","Number"),	//Creates an invalid Number instance
    Member("number_collection","Number_Collection"),	//Creates an invalid Number Collection instance
    Member("number_map","Number_Map"),	//Creates an invalid Number Map instance
    Member("page","Page"),	//Creates an invalid Page instance
    Member("page_button","Page_Button"),	//Creates an invalid Page Button instance
    Member("page_collection","Page_Collection"),	//Creates an invalid Page Collection instance
    Member("picture","Picture"),	//Creates an invalid Picture instance
    Member("picture_album","Picture_Album"),	//Creates an invalid Picture Album instance
    Member("picture_albums","Picture_Albums"),	//Creates an invalid Picture Albums instance
    Member("pictures","Pictures"),	//Creates an invalid Pictures instance
    Member("place","Place"),	//Creates an invalid Place instance
    Member("place_collection","Place_Collection"),	//Creates an invalid Place Collection instance
    Member("playlist","Playlist"),	//Creates an invalid Playlist instance
    Member("playlists","Playlists"),	//Creates an invalid Playlists instance
    Member("printer","Printer"),	//Creates an invalid Printer instance
    Member("printer_collection","Printer_Collection"),	//Creates an invalid Printer Collection instance
    Member("song","Song"),	//Creates an invalid Song instance
    Member("song_album","Song_Album"),	//Creates an invalid Song Album instance
    Member("song_albums","Song_Albums"),	//Creates an invalid Song Albums instance
    Member("songs","Songs"),	//Creates an invalid Songs instance
    Member("sound","Sound"),	//Creates an invalid Sound instance
    Member("sprite","Sprite"),	//Creates an invalid Sprite instance
    Member("sprite_set","Sprite_Set"),	//Creates an invalid Sprite Set instance
    Member("string","String"),	//Creates an invalid String instance
    Member("string_collection","String_Collection"),	//Creates an invalid String Collection instance
    Member("string_map","String_Map"),	//Creates an invalid String Map instance
    Member("textbox","TextBox"),	//Creates an invalid TextBox instance
    Member("tile","Tile"),	//Creates an invalid Tile instance
    Member("vector3","Vector3"),	//Creates an invalid Vector3 instance
    Member("web_request","Web_Request"),	//Creates an invalid Web Request instance
    Member("web_response","Web_Response"),	//Creates an invalid Web Response instance
    Member("xml_object","Xml_Object")	//Creates an invalid Xml Object instance
  ))
  addSingleton("languages", GenericTypes.gAlsoSingletons("languages") :::List(	// Translation, speech to text, ...
    Member("current_language","String"),	//Gets the current language code, to be used in the 'translate' Member.
    Member("detect_language",List("String"),"String"),	//Automatically detects the language of a given text using Bing.
    Member("picture_to_text",List("String","Picture"),"String"),	//Extracts text in the picture using Project Hawaii from Microsoft Research.
    Member("record_text","String"),	//Converts the microphone dictation to text using Project Hawaii from Microsoft Research.
    Member("speak",List("String","String"),"Sound"),	//Speaks the text in the specified language using Bing.
    Member("speech_to_text",List("String","Sound"),"String"),	//Converts a sound to a text using Project Hawaii from Microsoft Research.
    Member("translate",List("String","String","String"),"String")	//Translates some text between two languages using Bing. Empty source language to auto-detect.
  ))
  addSingleton("locations", GenericTypes.gAlsoSingletons("locations") :::List(	// Geo coordinates
    Member("create_location",List("Number","Number"),"Location"),	//Creates a new geo coordinate location
    Member("create_location_list","Location_Collection"),	//Creates an empty list of locations
    Member("describe_location",List("Location"),"String"),	//Looks for an address near a location using Bing.
    Member("search_location",List("String","String","String","String"),"Location")	//Looks for the coordinate of an address using Bing.
  ))
  addSingleton("maps", GenericTypes.gAlsoSingletons("maps") :::List(	// Maps, location to address, address to location
    Member("create_full_map","Map"),	//Creates a full screen Bing map. Use 'post to wall' to display it.
    Member("create_map","Map"),	//Creates a Bing map. Use 'post to wall' to display it.
    Member("directions",List("Location","Location","Boolean"),"Location_Collection"),	//Calculates the directions between two coordinates using Bing.
    Member("open_directions",List("String","Location","String","Location"),"Nothing"),	//Shows the directions in the Bing map application. If search term is provided, location is ignored.Provide search term or location for start and end.
    Member("open_map",List("Location","String","Number"),"Nothing")	//Opens the Bing map application. zoom between 0 (close) and 1 (far).
  ))
  addSingleton("math", GenericTypes.gAlsoSingletons("math") :::List(	// Mathematical operators, cos, sin, ...
    Member("∞₋","Number"),	//Returns the negative infinity
    Member("∞₊","Number"),	//Returns the positive infinity
    Member("abs",List("Number"),"Number"),	//Returns the absolute value of a number
    Member("acos",List("Number"),"Number"),	//Returns the angle whose cosine is the specified number
    Member("asin",List("Number"),"Number"),	//Returns the angle whose sine is the specified number
    Member("atan",List("Number"),"Number"),	//Returns the angle whose tangent is the specified number
    Member("atan2",List("Number","Number"),"Number"),	//Returns the angle whose tangent is the quotient of two specified numbers
    Member("ceiling",List("Number"),"Number"),	//Returns the smallest integral value greater than or equal to the specified number
    Member("cos",List("Number"),"Number"),	//Returns the cosine of the specified angle
    Member("cosh",List("Number"),"Number"),	//Returns the hyperbolic cosine of the specified angle
    Member("create_vector3",List("Number","Number","Number"),"Vector3"),	//Creates a 3D vector
    Member("deg_to_rad",List("Number"),"Number"),	//Converts degrees into radians
    Member("e","Number"),	//Returns the natural logarithmic base, specified by the constant, e
    Member("exp",List("Number"),"Number"),	//Returns e raised to the specified power
    Member("floor",List("Number"),"Number"),	//Returns the largest integer less than or equal to the specified number
    Member("gravity","Number"),	//Returns the gravity constant (9.80665)
    Member("ieee_remainder",List("Number","Number"),"Number"),	//Returns the remainder resulting from the division of a specified number by another specified number
    Member("is_∞",List("Number"),"Boolean"),	//Indicates whether number evaluates to negative or positive infinity
    Member("is_∞₋",List("Number"),"Boolean"),	//Indicates whether number evaluates to negative infinity
    Member("is_∞₊",List("Number"),"Boolean"),	//Indicates whether number evaluates to positive infinity
    Member("is_nan",List("Number"),"Boolean"),	//Indicates that value cannot be represented as a number, i.e. Not-a-Number. This usually happens when the number is the result of a division by zero.
    Member("log",List("Number","Number"),"Number"),	//Returns the logarithm of a specified number in a specified base
    Member("log10",List("Number"),"Number"),	//Returns the base 10 logarithm of a specified number
    Member("loge",List("Number"),"Number"),	//Returns the natural (base e) logarithm of a specified number
    Member("max",List("Number","Number"),"Number"),	//Returns the larger of two numbers
    Member("min",List("Number","Number"),"Number"),	//Returns the smaller of two numbers
    Member("mod",List("Number","Number"),"Number"),	//Returns the modulus resulting from the division of one number by another number
    Member("pow",List("Number","Number"),"Number"),	//Returns a specified number raised to the specified power
    Member("rad_to_deg",List("Number"),"Number"),	//Converts rad into degrees
    Member("random",List("Number"),"Number"),	//Returns a random integral number x: 0 â‰¤ x < max
    Member("random_normalized","Number"),	//Returns a random floating-point number x: 0 â‰¤ x < 1
    Member("round",List("Number"),"Number"),	//Rounds a number to the nearest integral value
    Member("round_with_precision",List("Number","Number"),"Number"),	//Rounds a number to a specified number of fractional digits.
    Member("sign",List("Number"),"Number"),	//Returns a value indicating the sign of a number
    Member("sin",List("Number"),"Number"),	//Returns the sine of the specified angle
    Member("sinh",List("Number"),"Number"),	//Returns the hyperbolic sine of the specified angle
    Member("sqrt",List("Number"),"Number"),	//Returns the square root of a specified number
    Member("tan",List("Number"),"Number"),	//Returns the tangent of the specified angle
    Member("tanh",List("Number"),"Number"),	//Returns the hyperbolic tangent of the specified angle
    Member("ε","Number"),	//Returns the smallest positive number greater than zero.
    Member("π","Number")	//Returns the Pi constant
  ))
  addSingleton("media", GenericTypes.gAlsoSingletons("media") :::List(	// Pictures and music...
    Member("choose_picture","Picture"),	//Chooses a picture from the media library
    Member("create_board",List("Number"),"Board"),	//Creates a new game board
    Member("create_full_board","Board"),	//Creates a new game board that will take the entire screen when posted.
    Member("create_picture",List("Number","Number"),"Picture"),	//Creates a new picture of the given size
    Member("icon",List("String"),"Picture"),	//Gets a 48x48 icon picture. Use 'media->icon names' to retrieve the list of names available.
    Member("icon_names","String_Collection"),	//Gets the list of built-in 48x48 icon names. You can see the icon list in the script settings.
    Member("large_icon",List("String"),"Picture"),	//Gets a 96x96 icon picture. Use 'media->icon names' to retrieve the list of names available.
    Member("picture_albums","Picture_Albums"),	//Gets the picture albums
    Member("pictures","Pictures"),	//Gets the pictures on the phone
    Member("playlists","Playlists"),	//Gets the playlists on the phone
    Member("saved_pictures","Pictures"),	//Gets the saved pictures on the phone
    Member("search_marketplace",List("String","String"),"Nothing"),	//Searches the Windows Phone Marketplace (type in applications or music)
    Member("song_albums","Song_Albums"),	//Gets the song albums on the phone
    Member("songs","Songs")	//Gets the songs on the phone
  ))
  addSingleton("phone", GenericTypes.gAlsoSingletons("phone") :::List(	// Phone numbers, vibrate, etc...
    Member("choose_address","Link"),	//Chooses an address from the contacts
    Member("choose_phone_number","Link"),	//Chooses a phone number from the contact list
    Member("dial_phone_number",List("String"),"Nothing"),	//Starts a phone call
    Member("power_source","String"),	//Indicates if the phone is on 'battery' or 'external' power source.
    Member("save_phone_number",List("String"),"Nothing"),	//Allows the user to save the phone number
    Member("vibrate",List("Number"),"Nothing")	//Vibrates the phone for ... seconds (0.02 minimum)
  ))
  addSingleton("player", GenericTypes.gAlsoSingletons("player") :::List(	// Play, stop or resume songs, ...
    Member("active_song","Song"),	//Gets the active song if any
    Member("is_muted","Boolean"),	//Indicates if the player is muted
    Member("is_paused","Boolean"),	//Indicates if the player is paused
    Member("is_playing","Boolean"),	//Indicates if the player is playing a song
    Member("is_repeating","Boolean"),	//Indicates if the player is repeating
    Member("is_shuffled","Boolean"),	//Indicates if the player is shuffled
    Member("is_stopped","Boolean"),	//Indicates if the player is stopped
    Member("next","Nothing"),	//Moves to the next song in the queue of playing songs
    Member("pause","Nothing"),	//Pauses the currently playing song
    Member("play",List("Song"),"Nothing"),	//Plays a Song
    Member("play_home_media",List("Media_Link"),"Nothing"),	//Plays an audio/video file from the home network
    Member("play_many",List("Songs"),"Nothing"),	//Plays a collection of songs
    Member("play_position","Number"),	//Gets the position in seconds whithin the active song
    Member("previous","Nothing"),	//Moves to the previous song in the queue of playing songs
    Member("resume","Nothing"),	//Resumes a paused song
    Member("set_repeating",List("Boolean"),"Nothing"),	//Sets the repeating on and off
    Member("set_shuffled",List("Boolean"),"Nothing"),	//Sets the shuffling on and off
    Member("set_sound_volume",List("Number"),"Nothing"),	//Sets the sound volume level from 0 (silent) to 1 (current volume)
    Member("sound_volume","Number"),	//Gets the sound volume for sounds from 0 (silent) to 1 (current volume)
    Member("stop","Nothing"),	//Stops playing a song
    Member("volume","Number")	//Gets player volume, from 0.0f (silence) to 1.0f (full volume relative to the current device volume). Setting the volume is no longer supported.
  ))
  addSingleton("radio", GenericTypes.gAlsoSingletons("radio") :::List(	// Access to the radio
    Member("frequency","Number"),	//Gets the frequency
    Member("is_playing","Boolean"),	//Indicates if the radio is on
    Member("link_frequency",List("String","Number"),"Link"),	//Creates a link to a radio frequency
    Member("set_frequency",List("Number"),"Nothing"),	//Sets the frequency
    Member("signal_strength","Number"),	//Gets the signal strength
    Member("start","Nothing"),	//Turns on the radio
    Member("stop","Nothing")	//Turns off the radio
  ))
  addSingleton("senses", GenericTypes.gAlsoSingletons("senses") :::List(	// Camera, location, microphone and other sensors
    Member("acceleration_quick","Vector3"),	//Gets filtered accelerometer data using a combination of a low-pass and threshold triggered high-pass on each axis to eliminate the majority of the sensor low amplitude noise while trending very quickly to large offsets (not perfectly smooth signal in that case), providing a very low latency. This is ideal for quickly reacting UI updates.
    Member("acceleration_smooth","Vector3"),	//Gets filtered accelerometer data using a 1 Hz first-order low-pass on each axis to eliminate the main sensor noise while providing a medium latency. This can be used for moderately reacting UI updates requiring a very smooth signal.
    Member("acceleration_stable","Vector3"),	//Gets filtered and temporally averaged accelerometer data using an arithmetic mean of the last 25 'optimally filtered' samples, so over 500ms at 50Hz on each axis, to virtually eliminate most sensor noise. This provides a very stable reading but it has also a very high latency and cannot be used for rapidly reacting UI.
    Member("camera","Camera"),	//Gets the primary camera
    Member("current_location","Location"),	//Gets the current phone location. The phone optimizes the accuracy for power, performance, and other cost considerations.
    Member("current_location_accurate","Location"),	//Gets the current phone location with the most accuracy. This includes using services that might charge money, or consuming higher levels of battery power or connection bandwidth.
    Member("front_camera","Camera"),	//Gets the front facing camera
    Member("has_accelerometer","Boolean"),	//Indicates whether accelerometer is present on device
    Member("has_compass","Boolean"),	//Indicates if the compass is available on the device
    Member("has_front_camera","Boolean"),	//Indicates if this device has a front facing camera
    Member("has_gyroscope","Boolean"),	//Indicates if the gyroscope is available on the device
    Member("has_motion","Boolean"),	//Indicates if the motion can be computed on the device. Motion requires accelerometer, compass and gyroscope.
    Member("heading","Number"),	//Gets the compass heading, in degrees, measured clockwise from the Earthâ€™s geographic north.
    Member("is_device_stable","Boolean"),	//Indicates whether the device is 'stable' (no movement for about 0.5 seconds)
    Member("motion","Motion"),	//Gets the current phone motion that combines data from the accelerometer, compass and gyroscope.
    Member("record_microphone","Sound"),	//Records audio using the microphone
    Member("rotation_speed","Vector3"),	//Gets the gyroscope rotational velocity around each axis of the device, in degrees per second.
    Member("take_camera_picture","Picture")	//Takes a picture and returns it. This picture does not contain the gps location.
  ))
  addSingleton("social", GenericTypes.gAlsoSingletons("social") :::List(	// Emails, sms, contacts, calendar, ...
    Member("choose_contact","Contact"),	//Chooses a contact from the contact list
    Member("choose_email","Link"),	//Chooses an email from the contact list
    Member("create_contact",List("String"),"Contact"),	//Creates a new contact
    Member("create_message",List("String"),"Message"),	//Creates a message to share
    Member("create_place",List("String","Location"),"Place"),	//Creates a place
    Member("link_email",List("String"),"Link"),	//Creates a link from an email
    Member("link_phone_number",List("String"),"Link"),	//Creates a link from a phone number
    Member("save_contact",List("Contact"),"Nothing"),	//Saves a new contact
    Member("save_email",List("String"),"Nothing"),	//Allows the user to save the email address (email)
    Member("search",List("String","String"),"Message_Collection"),	//Searches for recent messages in a social network (twitter, facebook)
    Member("search_appointments",List("DateTime","DateTime"),"Appointment_Collection"),	//Searches for appointments in a given time range
    Member("search_contacts",List("String"),"Contact_Collection"),	//Searches for contacts by name.
    Member("search_places_nearby",List("String","String","Location","Number"),"Place_Collection"),	//Searches for places nearby. The distance is in meters.
    Member("send_email",List("String","String","String"),"Nothing"),	//Opens the mail client
    Member("send_sms",List("String","String"),"Nothing")	//Opens the short message client (to, body)
  ))
  addSingleton("tags", GenericTypes.gAlsoSingletons("tags") :::List(	// 2D barcode generation and scanning
    Member("tag_text",List("String","Number","Boolean"),"Picture"),	//Generates a 2D barcode pointing to the text using Microsoft Tag. text must be less than 1000 character long and size must be between 0.75 and 5 inches.
    Member("tag_url",List("String","Number","Boolean"),"Picture")	//Generates a 2D barcode pointing to the url using Microsoft Tag. url must be less than 1000 character long and size must be between 0.75 and 5 inches.
  ))
  addSingleton("time", GenericTypes.gAlsoSingletons("time") :::List(	// time and dates
    Member("create",List("Number","Number","Number","Number","Number","Number"),"DateTime"),	//Creates a new date instance
    Member("fail_if_not",List("Boolean"),"Nothing"),	//Aborts the execution if the condition is false.
    Member("log",List("String"),"Nothing"),	//Appends this message to the debug log. Does nothing when the script is published.
    Member("now","DateTime"),	//Gets the current time
    Member("sleep",List("Number"),"Nothing"),	//Waits for a specified amount of seconds
    Member("stop","Nothing"),	//Stops the execution and stays on the wall.
    Member("stop_and_close","Nothing"),	//Stops the execution and leaves the wall.
    Member("today","DateTime"),	//Gets today's date without time
    Member("tomorrow","DateTime")	//Gets tomorrow's date without time
  ))
  addSingleton("wall", GenericTypes.gAlsoSingletons("wall") :::List(	// Ask or display values on the wall...
    Member("add_button",List("String","String"),"Page_Button"),	//Add a new button. icon must be the name of a built-in icon, text must be non-empty.
    Member("ask_boolean",List("String","String"),"Boolean"),	//Prompts the user with ok and cancel buttons
    Member("ask_number",List("String"),"Number"),	//Prompts the user to input a number
    Member("ask_string",List("String"),"String"),	//Prompts the user to input a string
    Member("button_icon_names","String_Collection"),	//Gets the list of available page button names.
    Member("clear","Nothing"),	//Clears the entries
    Member("clear_buttons","Nothing"),	//Clears the application bar buttons and hides the bar
    Member("create_text_box",List("String","Number"),"TextBox"),	//Creates an updatable text box
    Member("current_page","Page"),	//Gets the current page displayed on the wall
    Member("display_search",List("Boolean"),"Nothing"),	//Indicates whether to show or hide the search icon
    Member("pages","Page_Collection"),	//Returns the current back stack of pages, starting from the current page to the bottom page.
    Member("pick_date",List("String","String"),"DateTime"),	//Prompts the user to pick a date. Returns a datetime whose date is set, the time is 12:00:00.
    Member("pick_string",List("String","String","String_Collection"),"Number"),	//Prompts the user to pick a string from a list. Returns the selected index.
    Member("pick_time",List("String","String"),"DateTime"),	//Prompts the user to pick a time. Returns a datetime whose time is set, the date is undefined.
    Member("pop_page","Boolean"),	//Pops the current page and restores the previous wall page. Returns false if already on the default page.
    Member("prompt",List("String"),"Nothing"),	//Prompts the user with a ok button
    Member("push_new_page","Page"),	//Pushes an empty page on the wall.
    Member("screenshot","Picture"),	//Takes a screenshot of the wall.
    Member("set_background",List("Color"),"Nothing"),	//Sets the wall background color.
    Member("set_background_picture",List("Picture"),"Nothing"),	//Sets the wall background picture. The picture will be resized and clipped to the screen background as needed.
    Member("set_foreground",List("Color"),"Nothing"),	//Sets the wall foreground color of elements.
    Member("set_reversed",List("Boolean"),"Nothing"),	//Reverses the elements on the wall and inserts new ones at the bottom.
    Member("set_subtitle",List("String"),"Nothing"),	//Sets the subtitle of the wall.
    Member("set_title",List("String"),"Nothing"),	//Sets the title of the wall.
    Member("set_transform_matrix",List("Number","Number","Number","Number","Number","Number"),"Nothing")	//Sets the 3x3 affine matrix transformation applied to the wall.
  ))
  addSingleton("web", GenericTypes.gAlsoSingletons("web") ::: List(	// Search and browse the web...
    Member("base64_decode",List("String"),"String"),	//Decodes a string that has been base64-encoded
    Member("base64_encode",List("String"),"String"),	//Converts a string into an base64-encoded string
    Member("browse",List("String"),"Nothing"),	//Opens a web browser to a url
    Member("connection_name","String"),	//Gets a name of the currently connected network servicing Internet requests
    Member("connection_type","String"),	//Gets the type of the network servicing Internet requests (unknown, none, ethernet, wifi, mobile)
    Member("create_request",List("String"),"Web_Request"),	//Creates a web request
    Member("download",List("String"),"String"),	//Downloads the content of an internet page (http get)
    Member("download_json",List("String"),"Json_Object"),	//Downloads a web service response as a JSON data structure (http get)
    Member("download_picture",List("String"),"Picture"),	//Downloads a picture from internet
    Member("download_song",List("String","String"),"Song"),	//Create a streamed song file from internet (download happens when playing)
    Member("download_sound",List("String"),"Sound"),	//Downloads a WAV sound file from internet
    Member("feed",List("String"),"Message_Collection"),	//Parses the newsfeed string (RSS 2.0 or Atom 1.0) into a message collection
    Member("html_decode",List("String"),"String"),	//Decodes a string that has been HTML-encoded
    Member("html_encode",List("String"),"String"),	//Converts a text string into an HTML-encoded string
    Member("is_connected","Boolean"),	//Indicates whether any network connection is available
    Member("json",List("String"),"Json_Object"),	//Parses the string as a json object
    Member("link_image",List("String"),"Link"),	//Creates a link to an internet image
    Member("link_media",List("String"),"Link"),	//Creates a link to an internet audio/video
    Member("link_url",List("String","String"),"Link"),	//Creates a link to an internet page
    Member("open_connection_settings",List("String"),"Nothing"),	//Opens a connection settings page (airplanemode, bluetooth, wiki, cellular)
    Member("play_media",List("String"),"Nothing"),	//Plays an internet audio/video in full screen
    Member("search",List("String"),"Link_Collection"),	//Searching the web using Bing
    Member("search_images",List("String"),"Link_Collection"),	//Searching images using Bing
    Member("search_images_nearby",List("String","Location","Number"),"Link_Collection"),	//Searching images near a location using Bing. Distance in meters, negative to ignore.
    Member("search_nearby",List("String","Location","Number"),"Link_Collection"),	//Searching the web near a location using Bing. Distance in meters, negative to ignore.
    Member("search_news",List("String"),"Link_Collection"),	//Searching news using Bing
    Member("search_news_nearby",List("String","Location","Number"),"Link_Collection"),	//Searching news near a location using Bing. Distance in meters, negative to ignore.
    Member("upload",List("String","String"),"String"),	//Uploads text to an internet page (http post)
    Member("upload_picture",List("String","Picture"),"String"),	//Uploads a picture to an internet page (http post)
    Member("url_decode",List("String"),"String"),	//Decodes a string that has been url-encoded
    Member("url_encode",List("String"),"String"),	//Converts a text string into an url-encoded string
    Member("xml",List("String"),"Xml_Object")	//Parses the string as a xml element
  ))
  addType("Appointment", GenericTypes.gAny("Appointment") ::: List(	// An calendar appointment
    Member("attendees","Contact_Collection"),	//Gets the list of attendees. Each contact contains a name and email address.
    Member("details","String"),	//Gets the details
    Member("end_time","DateTime"),	//Gets the end time
    Member("is_all_day_event","Boolean"),	//Indicates if this is an all day event
    Member("is_private","Boolean"),	//Indicates if this appointment is private
    Member("location","String"),	//Gets the location
    Member("organizer","Contact"),	//Gets the organizer
    Member("source","String"),	//Gets the source of this appointment (facebook, etc...)
    Member("start_time","DateTime"),	//Gets the location
    Member("status","String"),	//Gets your status (free, tentative, busy, outofoffice)
    Member("subject","String")	//Gets the subject
  ))
  addType("Appointment_Collection", GenericTypes.gAny("Appointment_Collection") ::: List(	// A collection of appointments
    Member("at",List("Number"),"Appointment"),	//Gets the appointment at index
    Member("count","Number")	//Gets the number of appointments
  ))
  addType("Board", GenericTypes.gAny("Board") ::: List(	// A board to build 2D games
    Member("at",List("Number"),"Sprite"),	//Gets the sprite indexed by i
    Member("clear_background_camera","Nothing"),	//Clears the background camera
    Member("clear_background_picture","Nothing"),	//Clear the background picture
    Member("clear_events","Nothing"),	//Clear all queued events related to this board
    Member("count","Number"),	//Gets the sprite count
    Member("create_anchor",List("Number","Number"),"Sprite"),	//Create an anchor sprite.
    Member("create_boundary",List("Number"),"Nothing"),	//Create walls around the board at the given distance.
    Member("create_ellipse",List("Number","Number"),"Sprite"),	//Create a new ellipse sprite.
    Member("create_obstacle",List("Number","Number","Number","Number","Number"),"Nothing"),	//Create a line obstacle with given start point, and given extent. Elasticity is 0 for sticky, 1 for complete bounce.
    Member("create_picture",List("Picture"),"Sprite"),	//Create a new picture sprite.
    Member("create_rectangle",List("Number","Number"),"Sprite"),	//Create a new rectangle sprite.
    Member("create_spring",List("Sprite","Sprite","Number"),"Nothing"),	//Create a spring between the two sprites.
    Member("create_sprite_set","Sprite_Set"),	//Create a new collection for sprites.
    Member("create_text",List("Number","Number","Number","String"),"Sprite"),	//Create a new text sprite.
    Member("evolve","Nothing"),	//Update positions of sprites on board.
    Member("height","Number"),	//Gets the height in pixels
    Member("set_background",List("Color"),"Nothing"),	//Sets the background color
    Member("set_background_camera",List("Camera"),"Nothing"),	//Sets the background camera
    Member("set_background_picture",List("Picture"),"Nothing"),	//Sets the background picture
    Member("set_debug_mode",List("Boolean"),"Nothing"),	//In debug mode, board displays speed and other info of sprites
    Member("set_friction",List("Number"),"Nothing"),	//Sets the default friction for sprites to a fraction of speed loss between 0 and 1
    Member("set_gravity",List("Number","Number"),"Nothing"),	//Sets the uniform acceleration vector for objects on the board to pixels/sec^2
    Member("touch_current","Vector3"),	//Current touch point
    Member("touch_end","Vector3"),	//Last touch end point
    Member("touch_start","Vector3"),	//Last touch start point
    Member("touch_velocity","Vector3"),	//Final touch velocity after touch ended
    Member("touched","Boolean"),	//True if board is touched
    Member("update_on_wall","Nothing"),	//Make updates visible.
    Member("width","Number")	//Gets the width in pixels
  ))
  addType("Boolean", GenericTypes.gAny("Boolean") ::: List(	// true or false
    Member("and",List("Boolean"),"Boolean"),	//Builds conjunction
    Member("not","Boolean"),	//Negates the boolean expression
    Member("or",List("Boolean"),"Boolean"),	//Builds disjunction
    Member("to_number","Number"),	//Converts true to 1 and false to 0
    Member("to_string","String")	//Converts a boolean to a string
  ))
  addType("Camera", GenericTypes.gAny("Camera") ::: List(	// The front or back camera
    Member("height","Number"),	//Gets the height of the camera image in pixels.
    Member("is_front","Boolean"),	//Indicates if this camera is in front of the phone; false if this is the primary (back) camera.
    Member("preview","Picture"),	//Takes a low quality picture from the camera.
    Member("width","Number")	//Gets the width of the camera image in pixels.
  ))
  addType("Color", GenericTypes.gAny("Color") ::: List(	// A argb color (alpha, red, green, blue)
    Member("A","Number"),	//Gets the alpha value (0.0-1.0)
    Member("B","Number"),	//Gets the blue value (0.0-1.0)
    Member("blend",List("Color"),"Color"),	//Composes a new color using alpha blending
    Member("brightness","Number"),	//Gets the brightness component of the color.
    Member("darken",List("Number"),"Color"),	//Makes a darker color by a delta between 0 and 1.
    Member("G","Number"),	//Gets the green value (0.0-1.0)
    Member("hue","Number"),	//Gets the hue component of the color.
    Member("lighten",List("Number"),"Color"),	//Makes a lighter color by a delta between 0 and 1.
    Member("make_transparent",List("Number"),"Color"),	//Creates a new color by changing the alpha channel from 0 (transparent) to 1 (opaque).
    Member("R","Number"),	//Gets the red value (0.0-1.0)
    Member("saturation","Number")	//Gets the saturation component of the color.
  ))
  addType("Contact", GenericTypes.gAny("Contact") ::: List(	// A contact
    Member("birthday","DateTime"),	//Gets the birth date if any.
    Member("company","String"),	//Gets the company name if any.
    Member("email","Link"),	//Gets the work or personal email if any
    Member("first_name","String"),	//Gets the first name if any.
    Member("home_address","String"),	//Gets the work address if any
    Member("home_phone","Link"),	//Gets the home phone number if any
    Member("job_title","String"),	//Gets the job title at the company if any.
    Member("last_name","String"),	//Gets the last name if any.
    Member("middle_name","String"),	//Gets the middle name if any.
    Member("mobile_phone","Link"),	//Gets the cell phone number if any
    Member("name","String"),	//Gets the display name (not used when saving contact)
    Member("nick_name","String"),	//Gets the nickname if any.
    Member("office","String"),	//Gets the office location at the company if any.
    Member("personal_email","Link"),	//Gets the personal email if any
    Member("phone_number","Link"),	//Gets the cell or work or home phone number if any
    Member("picture","Picture"),	//Gets the picture of the contact if any.
    Member("set_company",List("String"),"Nothing"),	//Sets the company
    Member("set_first_name",List("String"),"Nothing"),	//Sets the first name
    Member("set_home_phone",List("String"),"Nothing"),	//Sets the home phone
    Member("set_job_title",List("String"),"Nothing"),	//Sets the job title
    Member("set_last_name",List("String"),"Nothing"),	//Sets the last name
    Member("set_middle_name",List("String"),"Nothing"),	//Sets the middle name
    Member("set_mobile_phone",List("String"),"Nothing"),	//Sets the mobile phone
    Member("set_nickname",List("String"),"Nothing"),	//Sets the nickname
    Member("set_personal_email",List("String"),"Nothing"),	//Sets the personal email
    Member("set_source",List("String"),"Nothing"),	//Sets the source
    Member("set_suffix",List("String"),"Nothing"),	//Sets the suffix
    Member("set_title",List("String"),"Nothing"),	//Sets the title
    Member("set_website",List("String"),"Nothing"),	//Sets the web site
    Member("set_work_email",List("String"),"Nothing"),	//Sets the work email
    Member("set_work_phone",List("String"),"Nothing"),	//Sets the work phone
    Member("source","String"),	//Gets the source of this contact (phone, etc...)
    Member("suffix","String"),	//Gets the name suffix if any.
    Member("title","String"),	//Gets the name title if any.
    Member("web_site","Link"),	//Gets the web site if any
    Member("work_address","String"),	//Gets the home address if any
    Member("work_email","Link"),	//Gets the work email if any
    Member("work_phone","Link")	//Gets the work phone number if any
  ))
  addType("Contact_Collection", GenericTypes.gAny("Contact_Collection") ::: List(	// A collection of contacts
    Member("at",List("Number"),"Contact"),	//Gets the contact at index
    Member("count","Number")	//Gets the number of contacts
  ))
  addType("DateTime", GenericTypes.gAny("DateTime") ::: List(	// A combination of date and time
    Member("add_days",List("Number"),"DateTime"),	//Returns a date that adds the specified number of days to the value of this instance.
    Member("add_hours",List("Number"),"DateTime"),	//Returns a date that adds the specified number of hours to the value of this instance.
    Member("add_milliseconds",List("Number"),"DateTime"),	//Returns a date that adds the specified number of milliseconds to the value of this instance.
    Member("add_minutes",List("Number"),"DateTime"),	//Returns a date that adds the specified number of minutes to the value of this instance.
    Member("add_months",List("Number"),"DateTime"),	//Returns a date that adds the specified number of months to the value of this instance.
    Member("add_seconds",List("Number"),"DateTime"),	//Returns a date that adds the specified number of seconds to the value of this instance.
    Member("add_years",List("Number"),"DateTime"),	//Returns a date that adds the specified number of years to the value of this instance.
    Member("date","DateTime"),	//Gets the date
    Member("day","Number"),	//Gets the day of the month
    Member("greater",List("DateTime"),"Boolean"),	//Compares dates for greater
    Member("greater_or_equal",List("DateTime"),"Boolean"),	//Compares dates for greater or equal
    Member("hour","Number"),	//Gets the hour
    Member("less",List("DateTime"),"Boolean"),	//Compares dates for less
    Member("less_or_equals",List("DateTime"),"Boolean"),	//Compares dates for less or equal
    Member("millisecond","Number"),	//Gets the millisecond
    Member("minute","Number"),	//Gets the minute
    Member("month","Number"),	//Gets the month
    Member("not_equals",List("DateTime"),"Boolean"),	//Compares dates for disequality
    Member("second","Number"),	//Gets the second
    Member("subtract",List("DateTime"),"Number"),	//Computes the difference between date-times in seconds
    Member("to_local_time","DateTime"),	//Converts to the local time
    Member("to_string","String"),	//Converts a dates to a string
    Member("to_universal_time","DateTime"),	//Converts coordinated universal time
    Member("week_day","Number"),	//Gets the day of the week (sunday = 0, monday = 1, ... saturday = 6)
    Member("year","Number"),	//Gets the year
    Member("year_day","Number")	//Gets the day of the year between 1 and 366
  ))
  addType("Device", GenericTypes.gAny("Device") ::: List(	// A device on the home network
    Member("browse","Nothing"),	//Browses to the device control panel
    Member("is_connected","Boolean"),	//Checks if the device is connected
    Member("manufacturer","String"),	//Gets the manfacturer name
    Member("name","String"),	//Gets the friendly name of the device
    Member("set_name",List("String"),"Nothing")	//Sets the friendly name of the device
  ))
  addType("Device_Collection", GenericTypes.gAny("Device_Collection") ::: List(	// A collection of devices
    Member("at",List("Number"),"Device"),	//Gets the device at index
    Member("count","Number")	//Gets the number of devices
  ))
  addType("Json_Object", GenericTypes.gAny("Json_Object") ::: List(	// A json data structure
    Member("at",List("Number"),"Json_Object"),	//Gets the i-th json value
    Member("boolean",List("String"),"Boolean"),	//Gets a Member value as a boolean
    Member("contains_key",List("String"),"Boolean"),	//Indicates if the key exists
    Member("count","Number"),	//Gets the number of values
    Member("field",List("String"),"Json_Object"),	//Gets a value by name
    Member("keys","String_Collection"),	//Gets the list of keys
    Member("kind","String"),	//Gets a json kind (string, number, object, array, boolean)
    Member("number",List("String"),"Number"),	//Gets a Member value as a number
    Member("string",List("String"),"String"),	//Gets a Member value as a string
    Member("time",List("String"),"DateTime"),	//Gets the Member value as a time
    Member("to_boolean","Boolean"),	//Converts to a boolean (type must be boolean)
    Member("to_number","Number"),	//Converts to a number (type must be number)
    Member("to_string","String"),	//Converts to a string (type must be string)
    Member("to_time","DateTime")	//Converts and parses to a date time (type must be string)
  ))
  addType("Link", GenericTypes.gAny("Link") ::: List(	// A link to a video, image, email, phone number
    Member("address","String"),	//Gets the url
    Member("kind","String"),	//Gets the kind of asset - media, image, email, phone number, hyperlink, deep zoom link, radio
    Member("location","Location"),	//Gets the location if any
    Member("name","String"),	//Gets the name if any
    Member("set_location",List("Location"),"Nothing"),	//Sets the location
    Member("set_name",List("String"),"Nothing"),	//Sets the name
    Member("share",List("String"),"Nothing")	//Shares the link (email, sms, facebook, social or '' to pick from a list)
  ))
  addType("Link_Collection", GenericTypes.gAny("Link_Collection") ::: List(	// A list of links
    Member("add",List("Link"),"Nothing"),	//Adds a link
    Member("add_many",List("Link_Collection"),"Nothing"),	//Adds many links at once
    Member("at",List("Number"),"Link"),	//Gets the i-th link
    Member("clear","Nothing"),	//Clears the collection
    Member("count","Number"),	//Gets the number of elements
    Member("index_of",List("Link","Number"),"Number"),	//Gets the index of the first occurrence of item. Returns -1 if not found or start is out of range.
    Member("insert_at",List("Number","Link"),"Nothing"),	//Inserts a link at position index. Does nothing if index is out of range.
    Member("random","Link"),	//Gets a random element from the collection. Returns invalid if the collection is empty.
    Member("remove",List("Link"),"Boolean"),	//Removes the first occurrence of the link. Returns true if removed.
    Member("remove_at",List("Number"),"Nothing"),	//Removes the link at position index.
    Member("reverse","Nothing"),	//Reverses the order of the elements.
    Member("set_at",List("Number","Link"),"Nothing")	//Sets the i-th link
  ))
  addType("Location", GenericTypes.gAny("Location") ::: List(	// A geo coordinate (latitude, longitude, ...)
    Member("altitude","Number"),	//Gets the altitude of the coordinate
    Member("course","Number"),	//Gets the course of the coordinate
    Member("distance",List("Location"),"Number"),	//Calculates the distance in meters
    Member("hor_accuracy","Number"),	//Gets the horizontal accuracy of the coordinate
    Member("latitude","Number"),	//Gets the latitude of the coordinate
    Member("longitude","Number"),	//Gets the longitude of the coordinate
    Member("share",List("String","String"),"Nothing"),	//Shares the location (email, sms, facebook, social or '' to pick from a list)
    Member("speed","Number"),	//Gets the speed of the coordinate
    Member("to_string","String"),	//Converts to a string lat,long
    Member("vert_accuracy","Number")	//Gets the vertical accuracy of the coordinate
  ))
  addType("Location_Collection", GenericTypes.gAny("Location_Collection") ::: List(	// A list of locations
    Member("add",List("Location"),"Nothing"),	//Adds a location
    Member("add_many",List("Location_Collection"),"Nothing"),	//Adds many locations at once
    Member("at",List("Number"),"Location"),	//Gets the i-th geo coordinate
    Member("clear","Nothing"),	//Clears the collection
    Member("count","Number"),	//Gets the number of elements
    Member("index_of",List("Location","Number"),"Number"),	//Gets the index of the first occurrence of item. Returns -1 if not found or start is out of range.
    Member("insert_at",List("Number","Location"),"Nothing"),	//Inserts a location at position index. Does nothing if index is out of range.
    Member("random","Location"),	//Gets a random element from the collection. Returns invalid if the collection is empty.
    Member("remove",List("Location"),"Boolean"),	//Removes the first occurrence of the location. Returns true if removed.
    Member("remove_at",List("Number"),"Nothing"),	//Removes the location at position index.
    Member("reverse","Nothing"),	//Reverses the order of the elements.
    Member("set_at",List("Number","Location"),"Nothing"),	//Sets the i-th geo coordinate
    Member("sort_by_distance",List("Location"),"Nothing")	//Sorts by distance to the location
  ))
  addType("Map", GenericTypes.gAny("Map") ::: List(	// A Bing map
    Member("add_line",List("Location_Collection","Color","Number"),"Nothing"),	//Adds a polyline that passes through various geocoordinates
    Member("add_link",List("Link","Color","Color"),"Nothing"),	//Adds a link pushpin on the map (ignored if the location if not set)
    Member("add_message",List("Message","Color","Color"),"Nothing"),	//Adds a message pushpin on the map (ignored if the location is not set)
    Member("add_picture",List("Location","Picture","Color"),"Nothing"),	//Adds a picture pushpin on the map
    Member("add_place",List("Place","Color","Color"),"Nothing"),	//Adds a place pushpin on the map (ignored if the location is not set)
    Member("add_text",List("Location","String","Color","Color"),"Nothing"),	//Adds a text pushpin on the map
    Member("center","Location"),	//Gets the map center location
    Member("clear","Nothing"),	//Clears the lines, regions and pushpins
    Member("fill_region",List("Location_Collection","Color","Color","Number"),"Nothing"),	//Fills a region with a color
    Member("set_center",List("Location"),"Nothing"),	//Sets the map center location
    Member("set_zoom",List("Number"),"Nothing"),	//Sets the zoom level from 1 (earth) to 21 (street)
    Member("view_pushpins","Nothing"),	//Changes the current zoom and center so that all the pushpins are visible. This Member has no effect if the map is not posted on a the wall yet.
    Member("zoom","Number")	//Gets the zoom level
  ))
  addType("Media_Link", GenericTypes.gAny("Media_Link") ::: List(	// A media file on the home network
    Member("album","String"),	//Gets the album if available
    Member("author","String"),	//Gets the author if available
    Member("date","DateTime"),	//Gets the date if available
    Member("duration","Number"),	//Gets the duration in seconds (0 for pictures)
    Member("kind","String"),	//Gets the kind of media (video, song, picture)
    Member("play","Nothing"),	//Plays or displays the media on the phone
    Member("title","String")	//Gets the title if available
  ))
  addType("Media_Link_Collection", GenericTypes.gAny("Media_Link_Collection") ::: List(	// A list of media links on the home network
    Member("at",List("Number"),"Media_Link"),	//Gets the i-th media link
    Member("count","Number")	//Gets the number of elements
  ))
  addType("Media_Player", GenericTypes.gAny("Media_Player") ::: List(	// An audio/video player on the home network
    Member("active_media","String"),	//Gets the uri of the media currently active
    Member("device","Device"),	//Gets the detailled information about this device
    Member("is_control_supported","Boolean"),	//Indicates the media can be played, paused, resumed
    Member("is_paused","Boolean"),	//Indicates if the player is paused
    Member("is_playing","Boolean"),	//Indicates if the player is playing
    Member("is_stopped","Boolean"),	//Indicates if the player is stopped
    Member("is_volume_supported","Boolean"),	//Indicates if volume can be changed
    Member("name","String"),	//Gets the name of the audio/video player
    Member("next","Nothing"),	//Moves the player to the next media in the queue.
    Member("pause","Nothing"),	//Pauses the current media if any.
    Member("play","Nothing"),	//Plays the current media from the start.
    Member("play_home_media",List("Media_Link"),"Nothing"),	//Plays a media from the home network.
    Member("play_media",List("String"),"Nothing"),	//Plays the media at the 'url' internet address.
    Member("play_position","Number"),	//Gets the position in seconds whithin the active media
    Member("previous","Nothing"),	//Moves the player to the previous media in the queue.
    Member("resume","Nothing"),	//Resumes playing the current media if any.
    Member("set_volume",List("Number"),"Nothing"),	//Sets the current value
    Member("status","String"),	//Gets the status of the player
    Member("stop","Nothing"),	//Stops the current media if any.
    Member("volume","Number")	//Gets the current volume
  ))
  addType("Media_Player_Collection", GenericTypes.gAny("Media_Player_Collection") ::: List(	// A collection of media players
    Member("at",List("Number"),"Media_Player"),	//Gets the media player at index
    Member("count","Number")	//Gets the number of media players
  ))
  addType("Media_Server", GenericTypes.gAny("Media_Server") ::: List(	// A media server on the home network
    Member("choose_picture","Media_Link"),	//Chooses a picture
    Member("choose_song","Media_Link"),	//Chooses a song
    Member("choose_video","Media_Link"),	//Chooses a video or a movie
    Member("device","Device"),	//Gets the detailled information about this device
    Member("name","String"),	//Gets the name of the printer
    Member("pictures","Media_Link_Collection"),	//Gets a list of all pictures
    Member("search_pictures_by_date",List("DateTime","DateTime"),"Media_Link_Collection"),	//Searches for pictures in a particular date range.
    Member("search_songs",List("String"),"Media_Link_Collection"),	//Searches for songs
    Member("search_videos",List("String"),"Media_Link_Collection"),	//Searches for videos
    Member("search_videos_by_date",List("DateTime","DateTime"),"Media_Link_Collection"),	//Searches for videos in a particular date range.
    Member("songs","Media_Link_Collection"),	//Gets a list of all songs
    Member("videos","Media_Link_Collection")	//Gets a list of all videos
  ))
  addType("Media_Server_Collection", GenericTypes.gAny("Media_Server_Collection") ::: List(	// A collection of media servers
    Member("at",List("Number"),"Media_Server"),	//Gets the media player at index
    Member("count","Number")	//Gets the number of media players
  ))
  addType("Message", GenericTypes.gAny("Message") ::: List(	// A post on a message board
    Member("from","String"),	//Gets the author
    Member("link","String"),	//Gets the link associated to the message
    Member("location","Location"),	//Gets the geo coordinates
    Member("media_link","String"),	//Gets a url to the media
    Member("message","String"),	//Gets the message text
    Member("picture_link","String"),	//Gets a url to the picture
    Member("set_from",List("String"),"Nothing"),	//Sets the author
    Member("set_link",List("String"),"Nothing"),	//Sets the link associated to the message
    Member("set_location",List("Location"),"Nothing"),	//Sets the geo coordinates
    Member("set_media_link",List("String"),"Nothing"),	//Sets the url to the media
    Member("set_message",List("String"),"Nothing"),	//Sets the message text
    Member("set_picture_link",List("String"),"Nothing"),	//Sets the url to the picture
    Member("set_source",List("String"),"Nothing"),	//Sets the source of this message
    Member("set_time",List("DateTime"),"Nothing"),	//Sets the time
    Member("set_title",List("String"),"Nothing"),	//Sets the title text
    Member("set_to",List("String"),"Nothing"),	//Sets the recipient
    Member("share",List("String"),"Nothing"),	//Shares this message (email, sms, facebook, social or '' to pick from a list)
    Member("source","String"),	//Gets the source of this message (Facebook, Twitter, etc...)
    Member("time","DateTime"),	//Gets the time
    Member("title","String"),	//Gets the title text
    Member("to","String"),	//Gets the recipient
    Member("values","String_Map")	//Gets the additional values stored in the message
  ))
  addType("Message_Collection", GenericTypes.gAny("Message_Collection") ::: List(	// A list of messages
    Member("add",List("Message"),"Nothing"),	//Adds a Message
    Member("add_many",List("Message_Collection"),"Nothing"),	//Adds a collection of Message items
    Member("at",List("Number"),"Message"),	//Gets the i-th Message
    Member("clear","Nothing"),	//Clears the collection
    Member("count","Number"),	//Gets the number of elements
    Member("index_of",List("Message","Number"),"Number"),	//Gets the index of the first occurrence of item. Returns -1 if not found or start is out of range.
    Member("insert_at",List("Number","Message"),"Nothing"),	//Inserts a link at position index. Does nothing if index is out of range.
    Member("random","Message"),	//Gets a random element from the collection. Returns invalid if the collection is empty.
    Member("remove",List("Message"),"Boolean"),	//Removes the first occurrence of the message. Returns true if removed.
    Member("remove_at",List("Number"),"Nothing"),	//Removes the message at position index.
    Member("reverse","Nothing"),	//Reverses the order of the elements.
    Member("set_at",List("Number","Message"),"Nothing"),	//Sets the i-th Message
    Member("sort_by_date","Nothing")	//Sorts from the newest to oldest
  ))
  addType("Motion", GenericTypes.gAny("Motion") ::: List(	// Describes the motion of the device
    Member("acceleration","Vector3"),	//Gets the linear acceleration of the device, in gravitational units.
    Member("gravity","Vector3"),	//Gets the gravity vector associated with this reading.
    Member("pitch","Number"),	//Gets the pitch of the attitude in degrees
    Member("roll","Number"),	//Gets the roll of the attitude in degrees
    Member("rotation_speed","Vector3"),	//Gets the device rotation speed in degrees per sec.
    Member("time","DateTime"),	//Gets a timestamp indicating the time at which the reading was calculated.
    Member("yaw","Number")	//Gets the yaw of the attitude in degrees
  ))
  addType("Nothing", Nil)	// Represents no value of interest
  addType("Number", GenericTypes.gAny("Number") ::: List(	// A number (possibly negative and/or fractional)
    Member("-",List("Number"),"Number"),	//Subtracts numbers
    Member("*",List("Number"),"Number"),	//Multiplies numbers
    Member("/",List("Number"),"Number"),	//Divides numbers
    Member("+",List("Number"),"Number"),	//Adds numbers
    Member("<",List("Number"),"Boolean"),	//Compares numbers for less
    Member("=",List("Number"),"Boolean"),	//Compares numbers for equality
    Member("≠",List("Number"),"Boolean"),	//Compares numbers for disequality
    Member(">",List("Number"),"Boolean"),	//Compares numbers for more
    Member("≤",List("Number"),"Boolean"),	//Compares numbers for less or equal
    Member("≥",List("Number"),"Boolean"),	//Compares numbers for more or equal
    Member("to_character","String"),	//Interprets a number as a unicode value and converts it to the single character string
    Member("to_color","Color"),	//Interprets the number as a ARGB (alpha, red, green, blue) color
    Member("to_string","String")	//Converts a number to a string
  ))
  addType("Number_Collection", GenericTypes.gAny("Number_Collection") ::: List(	// A collection of numbers
    Member("add",List("Number"),"Nothing"),	//Adds a number at the end of the collection
    Member("add_many",List("Number_Collection"),"Nothing"),	//Adds many numbers at once
    Member("at",List("Number"),"Number"),	//Gets the number at position index. Returns invalid if index is out of range
    Member("avg","Number"),	//Computes the average of the values
    Member("clear","Nothing"),	//Clears the numbers
    Member("contains",List("Number"),"Boolean"),	//Indicates if the collection contains the item
    Member("count","Number"),	//Gets the number of items
    Member("index_of",List("Number","Number"),"Number"),	//Gets the index of the first occurrence of a number. Returns -1 if not found or start is out of range.
    Member("insert_at",List("Number","Number"),"Nothing"),	//Inserts a double at position index. Does nothing if index is out of range.
    Member("max","Number"),	//Computes the maximum of the values
    Member("min","Number"),	//Computes the minimum of the values
    Member("random","Number"),	//Gets a random element from the collection. Returns invalid if the collection is empty.
    Member("remove",List("Number"),"Boolean"),	//Removes the first occurrence of a number. Returns true if removed.
    Member("remove_at",List("Number"),"Nothing"),	//Removes the number at position index.
    Member("reverse","Nothing"),	//Reverses the items
    Member("set_at",List("Number","Number"),"Nothing"),	//Sets the number at position index. Does nothing if the index is out of range.
    Member("sort","Nothing"),	//Sorts the numbers in this collection
    Member("sum","Number")	//Computes the sum of the values
  ))
  addType("Number_Map", GenericTypes.gAny("Number_Map") ::: List(	// A map of numbers to numbers
    Member("at",List("Number"),"Number"),	//Gets the element at index. Index may be any floating-point value.
    Member("avg","Number"),	//Computes the average of the values
    Member("count","Number"),	//Gets the number of elements
    Member("max","Number"),	//Computes the maximum of the values
    Member("min","Number"),	//Computes the minimum of the values
    Member("remove",List("Number"),"Nothing"),	//Removes the value at a given index
    Member("set_at",List("Number","Number"),"Nothing"),	//Sets the element at index. Index may be any floating-point value.
    Member("set_many",List("Number_Map"),"Nothing"),	//Sets many elements at once.
    Member("slice",List("Number","Number"),"Number_Map"),	//Extracts the elements at indices between start (inclusive) and end (non-inclusive).
    Member("sum","Number"),	//Computes the sum of the values
    Member("update_on_wall","Nothing")	//Updates any display of this map
  ))
  addType("Page", GenericTypes.gAny("Page") ::: List(	// A page on a wall
  ))
  addType("Page_Button", GenericTypes.gAny("Page_Button") ::: List(	// A page button on the wall
    Member("icon","String"),	//Gets the icon name
    Member("page","Page"),	//Gets the page hosting this button
    Member("text","String")	//Gets the text
  ))
  addType("Page_Collection", GenericTypes.gAny("Page_Collection") ::: List(	// A collection of page
    Member("at",List("Number"),"Page"),	//Gets the pages at index
    Member("count","Number")	//Gets the number of pages
  ))
  addType("Picture", GenericTypes.gAny("Picture") ::: List(	// A picture
    Member("at",List("Number"),"Color"),	//Gets the pixel color at the given linear index
    Member("blend",List("Picture","Number","Number","Number","Number"),"Nothing"),	//Writes another picture at a given location. The opacity ranges from 0 (transparent) to 1 (opaque).
    Member("brightness",List("Number"),"Nothing"),	//Changes the brightness of the picture. factor in [-1, 1].
    Member("clear",List("Color"),"Nothing"),	//Clears the picture to a given color
    Member("clone","Picture"),	//Returns a copy of the image
    Member("colorize",List("Color","Color","Number"),"Nothing"),	//Recolors the picture with the background and foreground color, based on a color threshold between 0.0 and 1.0
    Member("contrast",List("Number"),"Nothing"),	//Changes the contrast of the picture. factor in [-1, 1].
    Member("count","Number"),	//Gets the number of pixels
    Member("crop",List("Number","Number","Number","Number"),"Nothing"),	//Crops a sub-image
    Member("date","DateTime"),	//Gets the date time where the picture was taken; if any.
    Member("desaturate","Nothing"),	//Makes picture gray
    Member("draw_ellipse",List("Number","Number","Number","Number","Number","Color","Number"),"Nothing"),	//Draws an elliptic border with a given color
    Member("draw_line",List("Number","Number","Number","Number","Color","Number"),"Nothing"),	//Draws a line between two points
    Member("draw_rect",List("Number","Number","Number","Number","Number","Color","Number"),"Nothing"),	//Draws a rectangle border with a given color
    Member("draw_text",List("Number","Number","String","Number","Number","Color"),"Nothing"),	//Draws some text border with a given color and font size
    Member("fill_ellipse",List("Number","Number","Number","Number","Number","Color"),"Nothing"),	//Fills a ellipse with a given color
    Member("fill_rect",List("Number","Number","Number","Number","Number","Color"),"Nothing"),	//Fills a rectangle with a given color
    Member("flip_horizontal","Nothing"),	//Flips the picture horizontally
    Member("flip_vertical","Nothing"),	//Flips the picture vertically
    Member("height","Number"),	//Gets the height in pixels
    Member("invert","Nothing"),	//Inverts the red, blue and green channels
    Member("is_panorama","Boolean"),	//Indicates if the picture width is greater than its height
    Member("location","Location"),	//Gets the location where the picture was taken; if any.
    Member("pixel",List("Number","Number"),"Color"),	//Gets the pixel color
    Member("resize",List("Number","Number"),"Nothing"),	//Resizes the picture to the given size in pixels
    Member("save_to_library","String"),	//Saves the picture to the 'saved pictures' album. Returns the file name.
    Member("set_pixel",List("Number","Number","Color"),"Nothing"),	//Sets the pixel color at a given pixel
    Member("share",List("String","String"),"Nothing"),	//Shares this message ('' to pick from a list)
    Member("tint",List("Color"),"Nothing"),	//Converts every pixel to gray and tints it with the given color.
    Member("update_on_wall","Nothing"),	//Refreshes the picture on the wall
    Member("width","Number")	//Gets the width in pixels
  ))
  addType("Picture_Album", GenericTypes.gAny("Picture_Album") ::: List(	// A picture album
    Member("albums","Picture_Albums"),	//Gets the children albums
    Member("name","String"),	//Gets the name of the album
    Member("pictures","Pictures")	//Gets the pictures
  ))
  addType("Picture_Albums", GenericTypes.gAny("Picture_Albums") ::: List(	// A collection of picture albums
    Member("at",List("Number"),"Picture_Album"),	//Gets the item at position 'index'; invalid if index is out of bounds
    Member("count","Number"),	//Gets the number of elements in the collection
    Member("random","Picture_Album")	//Gets a random item; invalid if collection is empty
  ))
  addType("Pictures", GenericTypes.gAny("Pictures") ::: List(	// A collection of pictures
    Member("at",List("Number"),"Picture"),	//Gets the item at position 'index'; invalid if index is out of bounds
    Member("count","Number"),	//Gets the number of elements in the collection
    Member("find",List("String"),"Number"),	//Finds a picture by name and returns the index. Returns -1 if not found.
    Member("random","Picture"),	//Gets a random item; invalid if collection is empty
    Member("thumbnail",List("Number"),"Picture")	//Gets the thumbnail of i-th picture.
  ))
  addType("Place", GenericTypes.gAny("Place") ::: List(	// A named location
    Member("category","String"),	//Gets the category of the place
    Member("check_in","Nothing"),	//Checks into the place (supported for Facebook)
    Member("link","String"),	//Gets the link associated to the message
    Member("location","Location"),	//Gets the location of the place
    Member("name","String"),	//Gets the name of the place
    Member("picture_link","String"),	//Gets a url to the picture
    Member("set_category",List("String"),"Nothing"),	//Sets the category of the place
    Member("set_link",List("String"),"Nothing"),	//Sets the link associated to the message
    Member("set_location",List("Location"),"Nothing"),	//Sets the location of the place
    Member("set_name",List("String"),"Nothing"),	//Sets the name of the place
    Member("set_picture_link",List("String"),"Nothing"),	//Sets the url to the picture
    Member("source","String"),	//Gets the source of this place (facebook, touchdevelop)
    Member("to_string","String")	//Converts to a string name,lat,long
  ))
  addType("Place_Collection", GenericTypes.gAny("Place_Collection") ::: List(	// A collection of places
    Member("add",List("Place"),"Nothing"),	//Adds a place
    Member("add_many",List("Place_Collection"),"Nothing"),	//Adds many places at once
    Member("at",List("Number"),"Place"),	//Gets the i-th place
    Member("clear","Nothing"),	//Clears the collection
    Member("count","Number"),	//Gets the number of elements
    Member("index_of",List("Place","Number"),"Number"),	//Gets the index of the first occurrence of item. Returns -1 if not found or start is out of range.
    Member("insert_at",List("Number","Place"),"Nothing"),	//Inserts a place at position index. Does nothing if index is out of range.
    Member("random","Place"),	//Gets a random element from the collection. Returns invalid if the collection is empty.
    Member("remove",List("Place"),"Boolean"),	//Removes the first occurrence of a place. Returns true if removed.
    Member("remove_at",List("Number"),"Nothing"),	//Removes the location at position index.
    Member("reverse","Nothing"),	//Reverses the order of the elements.
    Member("set_at",List("Number","Place"),"Nothing"),	//Sets the i-th place
    Member("sort_by_distance",List("Location"),"Nothing")	//Sorts the places by distance to the location
  ))
  addType("Playlist", GenericTypes.gAny("Playlist") ::: List(	// A song playlist
    Member("duration","Number"),	//Gets the duration in seconds
    Member("name","String"),	//Gets the name of the song
    Member("play","Nothing"),	//Plays the songs in the playlist
    Member("songs","Songs")	//Gets the songs
  ))
  addType("Playlists", GenericTypes.gAny("Playlists") ::: List(	// A collection of playlists
    Member("at",List("Number"),"Playlist"),	//Gets i-th playlist
    Member("count","Number")	//Gets the number of playlists
  ))
  addType("Printer", GenericTypes.gAny("Printer") ::: List(	// A printer on the home network
    Member("device","Device"),	//Gets the detailled information about this device
    Member("is_idle","Boolean"),	//Indicates if new jobs can start processing immediately without waiting.
    Member("is_processing","Boolean"),	//Indicates if jobs are processing; new jobs will wait before processing, i.e., are said to be pending.
    Member("is_stopped","Boolean"),	//Indicates if no jobs can be processed and intervention is needed.
    Member("name","String"),	//Gets the name of the printer
    Member("print_text",List("String"),"Nothing"),	//Queues a job to print the text.
    Member("state_reason","String")	//Indicates additional information about why the Printer is in its current state.
  ))
  addType("Printer_Collection", GenericTypes.gAny("Printer_Collection") ::: List(	// A collection of printers
    Member("at",List("Number"),"Printer"),	//Gets the printer at index
    Member("count","Number")	//Gets the number of printers
  ))
  addType("Song", GenericTypes.gAny("Song") ::: List(	// A song
    Member("album","Song_Album"),	//Gets the song album containing the song
    Member("artist","String"),	//Gets the name of the artist
    Member("duration","Number"),	//Gets the duration in seconds
    Member("genre","String"),	//Gets the genre of the song
    Member("name","String"),	//Gets the name of the song
    Member("play","Nothing"),	//Plays the song.
    Member("play_count","Number"),	//Gets the play count
    Member("protected","Boolean"),	//Gets a value whether the song is DRM protected
    Member("rating","Number"),	//Gets the users rating. -1 if not rated.
    Member("track","Number")	//Gets the track number in the album
  ))
  addType("Song_Album", GenericTypes.gAny("Song_Album") ::: List(	// A song album
    Member("art","Picture"),	//Gets album art picture
    Member("artist","String"),	//Gets the name of the artist
    Member("duration","Number"),	//Gets the duration in seconds
    Member("genre","String"),	//Gets the genre of the song
    Member("has_art","Boolean"),	//Indicates if the album has art
    Member("name","String"),	//Gets the name of the album
    Member("play","Nothing"),	//Plays the songs of the album
    Member("songs","Songs"),	//Gets the songs
    Member("thumbnail","Picture")	//Gets the thumbnail picture
  ))
  addType("Song_Albums", GenericTypes.gAny("Song_Albums") ::: List(	// A collection of albums
    Member("at",List("Number"),"Song_Album"),	//Gets the item at position 'index'; invalid if index is out of bounds
    Member("count","Number"),	//Gets the number of elements in the collection
    Member("random","Song_Album")	//Gets a random item; invalid if collection is empty
  ))
  addType("Songs", GenericTypes.gAny("Songs") ::: List(	// A collection of songs
    Member("at",List("Number"),"Song"),	//Gets the item at position 'index'; invalid if index is out of bounds
    Member("count","Number"),	//Gets the number of elements in the collection
    Member("play","Nothing"),	//Plays the song.
    Member("random","Song")	//Gets a random item; invalid if collection is empty
  ))
  addType("Sound", GenericTypes.gAny("Sound") ::: List(	// A sound effect
    Member("duration","Number"),	//Gets the duration in seconds.
    Member("pan","Number"),	//Gets the panning, ranging from -1.0 (full left) to 1.0 (full right).
    Member("pitch","Number"),	//Gets the pitch adjustment, ranging from -1 (down one octave) to 1 (up one octave).
    Member("play","Nothing"),	//Plays the sound effect
    Member("play_special",List("Number","Number","Number"),"Nothing"),	//Plays the song with different volume (0 to 1), pitch (-1 to 1) and pan (-1 to 1).
    Member("set_pan",List("Number"),"Nothing"),	//Sets the panning, ranging from -1.0 (full left) to 1.0 (full right).
    Member("set_pitch",List("Number"),"Nothing"),	//Sets the pitch adjustment, ranging from -1 (down one octave) to 1 (up one octave).
    Member("set_volume",List("Number"),"Nothing"),	//Sets the volume from 0 (silent) to 1 (full volume).
    Member("volume","Number")	//Gets the volume from 0 (silent) to 1 (full volume)
  ))
  addType("Sprite", GenericTypes.gAny("Sprite") ::: List(	// A sprite
    Member("acceleration_x","Number"),	//Gets the acceleration along x in pixels/sec^2
    Member("acceleration_y","Number"),	//Gets the acceleration along y in pixels/sec^2
    Member("angle","Number"),	//Gets the angle of the sprite in degrees
    Member("angular_speed","Number"),	//Gets the rotation speed in degrees/sec
    Member("color","Color"),	//Returns the sprite color.
    Member("delete","Nothing"),	//Delete sprite.
    Member("elasticity","Number"),	//Gets the sprite elasticity as a fraction of speed preservation per bounce (0-1)
    Member("friction","Number"),	//Gets the fraction of speed loss between 0 and 1
    Member("height","Number"),	//Gets the height in pixels
    Member("hide","Nothing"),	//Hide sprite.
    Member("is_visible","Boolean"),	//Returns true if sprite is not hidden
    Member("location","Location"),	//Gets the geo location assigned to the sprite
    Member("mass","Number"),	//Gets the mass
    Member("move",List("Number","Number"),"Nothing"),	//Moves sprite.
    Member("move_clip",List("Number","Number"),"Nothing"),	//Moves the clipping area and wraps around the image if needed (if it is an image sprite)
    Member("move_towards",List("Sprite","Number"),"Nothing"),	//Moves sprite towards other sprite.
    Member("opacity","Number"),	//Gets the opacity (between 0 transparent and 1 opaque)
    Member("overlap_with",List("Sprite_Set"),"Sprite_Set"),	//Returns the subset of sprites in the given set that overlap with sprite.
    Member("overlaps_with",List("Sprite"),"Boolean"),	//Do the sprites overlap
    Member("set_acceleration",List("Number","Number"),"Nothing"),	//Sets the acceleration in pixels/sec^2
    Member("set_acceleration_x",List("Number"),"Nothing"),	//Sets the x acceleration in pixels/sec^2
    Member("set_acceleration_y",List("Number"),"Nothing"),	//Sets the y acceleration in pixels/sec^2
    Member("set_angle",List("Number"),"Nothing"),	//Sets the angle of the sprite in degrees
    Member("set_angular_speed",List("Number"),"Nothing"),	//Sets the rotation speed in degrees/sec
    Member("set_clip",List("Number","Number","Number","Number"),"Nothing"),	//Sets the clipping area for an image sprite (if it is an image sprite)
    Member("set_color",List("Color"),"Nothing"),	//Sets the sprite color.
    Member("set_elasticity",List("Number"),"Nothing"),	//Sets the sprite elasticity as a fraction of speed preservation per bounce (0-1)
    Member("set_friction",List("Number"),"Nothing"),	//Sets the friction to a fraction of speed loss between 0 and 1
    Member("set_height",List("Number"),"Nothing"),	//Sets the height in pixels
    Member("set_location",List("Location"),"Nothing"),	//Sets the geo location of the sprite
    Member("set_mass",List("Number"),"Nothing"),	//Sets the sprite mass.
    Member("set_opacity",List("Number"),"Nothing"),	//Sets the sprite opacity (between 0 transparent and 1 opaque).
    Member("set_picture",List("Picture"),"Nothing"),	//Updates picture on a picture sprite (if it is a picture sprite)
    Member("set_pos",List("Number","Number"),"Nothing"),	//Sets the position in pixels
    Member("set_speed",List("Number","Number"),"Nothing"),	//Sets the speed in pixels/sec
    Member("set_speed_x",List("Number"),"Nothing"),	//Sets the x speed in pixels/sec
    Member("set_speed_y",List("Number"),"Nothing"),	//Sets the y speed in pixels/sec
    Member("set_text",List("String"),"Nothing"),	//Updates text on a text sprite (if it is a text sprite)
    Member("set_width",List("Number"),"Nothing"),	//Sets the width in pixels
    Member("set_x",List("Number"),"Nothing"),	//Sets the x position in pixels
    Member("set_y",List("Number"),"Nothing"),	//Sets the y position in pixels
    Member("show","Nothing"),	//Show sprite.
    Member("speed_towards",List("Sprite","Number"),"Nothing"),	//Sets sprite speed direction towards other sprite with given magnitude.
    Member("speed_x","Number"),	//Gets the speed along x in pixels/sec
    Member("speed_y","Number"),	//Gets the speed along y in pixels/sec
    Member("text","String"),	//The text on a text sprite (if it is a text sprite)
    Member("width","Number"),	//Gets the width in pixels
    Member("x","Number"),	//Gets the x position in pixels
    Member("y","Number")	//Gets the y position in pixels
  ))
  addType("Sprite_Set", GenericTypes.gAny("Sprite_Set") ::: List(	// A collection of sprites
    Member("add",List("Sprite"),"Boolean"),	//Add sprite to set. Returns true if sprite was not already in set.
    Member("add_from",List("Sprite_Set","Sprite"),"Boolean"),	//Add sprite to set and remove from old set. Returns true if sprite was in old set and not in new set.
    Member("at",List("Number"),"Sprite"),	//Return sprite at given index.
    Member("contains",List("Sprite"),"Boolean"),	//Returns true if sprite is in set.
    Member("count","Number"),	//Returns the number of sprites in the set
    Member("index_of",List("Sprite"),"Number"),	//Returns index of sprite in this set or -1 if not in set.
    Member("remove",List("Sprite"),"Boolean"),	//Remove sprite from set. Returns true if sprite was in set.
    Member("remove_first","Sprite")	//Remove sprite that was added to set first.
  ))
  addType("String", GenericTypes.gAny("String") ::: List(	// A piece of text
    Member("at",List("Number"),"String"),	//Gets the character at a specified index
    Member("compare",List("String"),"Number"),	//Compares two pieces of text
    Member("concat",List("String"),"String"),	//Concatenates two pieces of text
    Member("contains",List("String"),"Boolean"),	//Returns a value indicating if the second string is contained
    Member("copy_to_clipboard","Nothing"),	//Stores text in the clipboard
    Member("count","Number"),	//Returns the number of characters
    Member("ends_with",List("String"),"Boolean"),	//Determines whether the ending matches the specified string
    Member("index_of",List("String","Number"),"Number"),	//Returns the index of the first occurrence if found starting at a given position
    Member("insert",List("Number","String"),"String"),	//Inserts a string at a given position
    Member("is_empty","Boolean"),	//Indicates if the string is empty
    Member("is_match_regex",List("String"),"Boolean"),	//Indicates if the string matches a regular expression
    Member("last_index_of",List("String","Number"),"Number"),	//Returns the index of the last occurrence if found starting at a given position
    Member("matches",List("String"),"String_Collection"),	//Gets the strings matching the regex expression (pattern)
    Member("remove",List("Number"),"String"),	//Returns all character from a string starting at a given index
    Member("replace",List("String","String"),"String"),	//Returns a given string with a replacement
    Member("replace_regex",List("String","String"),"String"),	//Replace every match of the regex according to the replacement string
    Member("share",List("String"),"Nothing"),	//Shares the string (email, sms, facebook, social or '' to pick from a list)
    Member("split",List("String"),"String_Collection"),	//Returns a string collection that contains the substrings in this string that are delimited by elements of a specified string.
    Member("starts_with",List("String"),"Boolean"),	//Determines whether the beginning matches the specified string
    Member("substring",List("Number","Number"),"String"),	//Returns a substring given a start index and a length
    Member("to_boolean","Boolean"),	//Parses the string as a boolean
    Member("to_color","Color"),	//Parses the string as a color.
    Member("to_datetime","DateTime"),	//Parses the string as a date and time.
    Member("to_location","Location"),	//Parses the string as a geo coordinate.
    Member("to_lower_case","String"),	//Returns a copy of this string converted to lowercase, using the casing rules of the current culture.
    Member("to_number","Number"),	//Parses the string as a number
    Member("to_time","Number"),	//Parses the string as a time (12:30:12) and returns the number of seconds.
    Member("to_unicode","Number"),	//Converts a single character string into its unicode number
    Member("to_upper_case","String"),	//Returns a copy of this string converted to uppercase, using the casing rules of the current culture.
    Member("trim",List("String"),"String"),	//Removes all leading and trailing occurrences of a set of characters specified in a string from the current string.
    Member("trim_end",List("String"),"String"),	//Removes all trailing occurrences of a set of characters specified in a string from the current string.
    Member("trim_start",List("String"),"String")	//Removes all leading occurrences of a set of characters specified in a string from the current string.
  ))
  addType("String_Collection", GenericTypes.gAny("String_Collection") ::: List(	// A collection of strings
    Member("add",List("String"),"Nothing"),	//Adds a string
    Member("add_many",List("String_Collection"),"Nothing"),	//Adds many strings at once
    Member("at",List("Number"),"String"),	//Gets the string at position index. Returns invalid if index is out of range
    Member("clear","Nothing"),	//Clears the strings
    Member("contains",List("String"),"Boolean"),	//Indicates if the collection contains the item
    Member("count","Number"),	//Gets the number of strings
    Member("index_of",List("String","Number"),"Number"),	//Gets the index of the first occurrence of a string. Returns -1 if not found or start is out of range.
    Member("insert_at",List("Number","String"),"Nothing"),	//Inserts a string at position index. Does nothing if index is out of range.
    Member("join",List("String"),"String"),	//Concatenates the separator and items into a string
    Member("random","String"),	//Gets a random element from the collection. Returns invalid if the collection is empty.
    Member("remove",List("String"),"Boolean"),	//Removes the first occurrence of a string. Returns true if removed.
    Member("remove_at",List("Number"),"Nothing"),	//Removes the string at position index.
    Member("reverse","Nothing"),	//Reverses the items
    Member("set_at",List("Number","String"),"Nothing"),	//Sets the string at position index. Does nothing if the index is out of range.
    Member("sort","Nothing")	//Sorts the strings in this collection
  ))
  addType("String_Map", GenericTypes.gAny("String_Map") ::: List(	// A map from strings to strings
    Member("at",List("String"),"String"),	//Gets the value at a given key; invalid if not found
    Member("count","Number"),	//Gets the number of elements in the map
    Member("keys","String_Collection"),	//Gets the keys in the map
    Member("remove",List("String"),"Nothing"),	//Removes the value at a given key
    Member("set_at",List("String","String"),"Nothing"),	//Sets the value at a given key; invalid if not found
    Member("set_many",List("String_Map"),"Nothing")	//Sets many elements at once.
  ))
  addType("TextBox", GenericTypes.gAny("TextBox") ::: List(	// A text box
    Member("background","Color"),	//Gets the background color
    Member("border","Color"),	//Gets the border color
    Member("font_size","Number"),	//Gets the font size
    Member("foreground","Color"),	//Gets the foreground color
    Member("icon","Picture"),	//Gets the icon picture (max 173x173)
    Member("set_background",List("Color"),"Nothing"),	//Sets the background color
    Member("set_border",List("Color"),"Nothing"),	//Sets the border color
    Member("set_font_size",List("Number"),"Nothing"),	//Sets the font size (small = 14, normal = 15, medium = 17, medium large = 19, large = 24, extra large = 32, extra extra large = 54, huge = 140
    Member("set_foreground",List("Color"),"Nothing"),	//Sets the foreground color
    Member("set_icon",List("Picture"),"Nothing"),	//Sets the icon picture (max 96 x 96)
    Member("set_text",List("String"),"Nothing"),	//Sets the text
    Member("text","String")	//Gets the text
  ))
  addType("Tile", GenericTypes.gAny("Tile") ::: List(	// A tile
    Member("back_icon","Picture"),	//Gets the back icon picture
    Member("back_title","String"),	//Gets the back title
    Member("background","Color"),	//Gets the background color
    Member("clear_back_icon","Nothing"),	//Clears the back icon image if any
    Member("clear_icon","Nothing"),	//Clears the front icon image if any
    Member("content","String"),	//Gets the content
    Member("counter","Number"),	//Gets the counter
    Member("height","Number"),	//Gets the height in pixels
    Member("icon","Picture"),	//Gets the icon picture
    Member("pin_to_start","Nothing"),	//Pins the tile to the start menu, after asking for user consent.
    Member("set_back_icon",List("Picture"),"Nothing"),	//Sets the back icon image, cropped to the tile size
    Member("set_back_title",List("String"),"Nothing"),	//Sets the back title.
    Member("set_background",List("Color"),"Nothing"),	//Sets the background color for the front and back tile.
    Member("set_content",List("String"),"Nothing"),	//Sets the text content that shows on the back tile.
    Member("set_counter",List("Number"),"Nothing"),	//Sets the counter; counters â‰¤ 0 are not displayed.
    Member("set_icon",List("Picture"),"Nothing"),	//Sets the front icon image, cropped to the tile size
    Member("set_title",List("String"),"Nothing"),	//Sets the front title. If empty, the tile will use the script name
    Member("title","String"),	//Gets the front title
    Member("width","Number")	//Gets the width in pixels
  ))
  addType("Vector3", GenericTypes.gAny("Vector3") ::: List(	// A 3D vector
    Member("add",List("Vector3"),"Vector3"),	//Adds a vector
    Member("clamp",List("Vector3","Vector3"),"Vector3"),	//Restricts the vector in the specified range
    Member("cross",List("Vector3"),"Vector3"),	//Calculates the cross product with the other vector
    Member("distance",List("Vector3"),"Number"),	//Gets the distance between the two vectors
    Member("length","Number"),	//Gets the length of the vector
    Member("linear_interpolation",List("Vector3","Number"),"Vector3"),	//Linear interpolation between two vectors
    Member("multiply",List("Vector3"),"Vector3"),	//Multiplies component-wise with a vector
    Member("negate","Vector3"),	//Returns a vector pointing in the opposite direction
    Member("normalize","Vector3"),	//Returns a vector of one unit pointing in the same direction as the original vector
    Member("scale",List("Number"),"Vector3"),	//Multiplies with a scaling factor
    Member("subtract",List("Vector3"),"Vector3"),	//Subtracts another vector
    Member("to_string","String"),	//Turns the vector into a string
    Member("x","Number"),	//Gets the x-component
    Member("y","Number"),	//Gets the y-component
    Member("z","Number")	//Gets the z-component
  ))
  addType("Web_Request", GenericTypes.gAny("Web_Request") ::: List(	// An HTTP web request
    Member("header",List("String"),"String"),	//Gets the value of a given header
    Member("header_names","String_Collection"),	//Gets the names of the headers
    Member("method","String"),	//Gets whether it was a 'get' or a 'post'.
    Member("send","Web_Response"),	//Performs the request synchronously
    Member("set_compress",List("Boolean"),"Nothing"),	//Compresses the request content with gzip and sets the Content-Encoding header
    Member("set_content",List("String"),"Nothing"),	//Sets the content of a 'post' request
    Member("set_content_as_json",List("Json_Object"),"Nothing"),	//Sets the content of a 'post' request as the JSON tree
    Member("set_content_as_picture",List("Picture","Number"),"Nothing"),	//Sets the content of a 'post' request as a JPEG encoded image. Quality from 0 (worse) to 1 (best).
    Member("set_content_as_xml",List("Xml_Object"),"Nothing"),	//Sets the content of a 'post' request as the XML tree
    Member("set_credentials",List("String","String"),"Nothing"),	//Sets the name and password for basic authentication. Requires an HTTPS URL, empty string clears.
    Member("set_header",List("String","String"),"Nothing"),	//Sets an HTML header value. Empty string clears the value
    Member("set_method",List("String"),"Nothing"),	//Sets the Member as 'get' or 'post'. Default value is 'get'.
    Member("set_url",List("String"),"Nothing"),	//Sets the url of the request. Must be a valid internet address.
    Member("url","String")	//Gets the url of the request
  ))
  addType("Web_Response", GenericTypes.gAny("Web_Response") ::: List(	// An HTTP web response
    Member("content","String"),	//Reads the response body as a string
    Member("content_as_json","Json_Object"),	//Reads the response body as a JSON tree
    Member("content_as_picture","Picture"),	//Reads the response body as a picture
    Member("content_as_sound","Sound"),	//Reads the response body as a wave sound
    Member("content_as_xml","Xml_Object"),	//Reads the response body as a XML tree
    Member("header",List("String"),"String"),	//Gets the value of a given header
    Member("header_names","String_Collection"),	//Gets the names of the headers
    Member("request","Web_Request"),	//Gets the request associated to this response
    Member("status_code","Number")	//Gets the HTTP Status code of the request if any
  ))
  addType("Xml_Object", GenericTypes.gAny("Xml_Object") ::: List(	// An xml element or collection of elements
    Member("at",List("Number"),"Xml_Object"),	//Gets the i-th child element in the collection
    Member("attr",List("String"),"String"),	//Gets the value of the attribute
    Member("attr_names","String_Collection"),	//Gets the list of attribute names
    Member("child",List("String"),"Xml_Object"),	//Gets a first child element matching the fully qualified name
    Member("children",List("String"),"Xml_Object"),	//Gets a collection of child element matching the fully qualified name
    Member("count","Number"),	//Gets the number of child element
    Member("create_name",List("String","String"),"String"),	//Creates a qualified full name from the namespace and local name
    Member("is_element","Boolean"),	//Indicates if this instance is an element or a filtered collection
    Member("local_name","String"),	//Gets the local name of this element
    Member("name","String"),	//Gets the full name of this element
    Member("namespace","String"),	//Gets the namespace of this element
    Member("to_string","String"),	//Gets an xml string
    Member("value","String")	//Gets the concatenated text contents of this element
  ))

}
