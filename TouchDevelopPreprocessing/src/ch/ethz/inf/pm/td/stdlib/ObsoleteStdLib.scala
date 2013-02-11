package ch.ethz.inf.pm.td.stdlib

import ch.ethz.inf.pm.td.symbols.{AbstractSymbolTable, Member}


/**
 *
 * Hidden and Obsolete elements (still contained in many scripts)
 *
 * Lucas Brutschy
 * Date: 8/20/12
 * Time: 5:45 PM
 *
 */
trait ObsoleteStdLib extends AbstractSymbolTable {


    addSingleton("assert",List( // This is not obsolete but my own stuff
      Member("is_true",List("Boolean"),"Nothing"),
      Member("is_false",List("Boolean"),"Nothing")
    ))
    addSingleton("bazaar",List(
      Member("open","Nothing"), // Launches the bazaar.
      Member("open_leaderboard","Nothing"), // Opens the leaderboard for the current script
      Member("open_review","Nothing") // Opens the review page for the current script
    ))
    addSingleton("colors",List(
      Member("rand","Color") // Renamed to 'random'
    ))
    addSingleton("media",List(
      Member("create_full_board","Board")	//Creates a new game board that will take the entire screen when posted.
    ))
    addSingleton("math",List(
      Member("create_number_map","Number_Map"), // Use Collections->create number map instead.
      Member("rand",List("Number"), "Number"), // Renamed to 'random'
      Member("rand_norm","Number") // Renamed to 'random normalized'
    ))
    addSingleton("player",List(
      Member("volume","Number")	//Gets player volume, from 0.0f (silence) to 1.0f (full volume relative to the current device volume). Setting the volume is no longer supported.
    ))
    addSingleton("social",List(
      Member("contacts",List("String"), "Contact_Collection") // Retrieves the list of contacts
    ))
    addSingleton("social",List(
      Member("has_accelerometer","Boolean"),	//Indicates whether accelerometer is present on device
      Member("has_compass","Boolean"),	//Indicates if the compass is available on the device
      Member("has_front_camera","Boolean"),	//Indicates if this device has a front facing camera
      Member("has_motion","Boolean")	//Indicates if the motion can be computed on the device. Motion requires accelerometer, compass and gyroscope.
    ))
    addSingleton("web",List(
      Member("link_deep_zoom",List("String"), "Link"), // Creates a multi-scale image from an image url
      Member("rss",List("String"), "Message_Collection"), // Obsolete. Use 'feed' instead.
      Member("search_phone_numbers",List("String"), "Link_Collection"), // Search phone numbers using Bing
      Member("search_phone_numbers_nearby",List("String", "Location", "Number"), "Link_Collection") // Search phone numbers near a location using Bing. Distance in meters, negative to ignore.
    ))
    addSingleton("wall",List(
      Member("icon_names","String_Collection") // Use button icon names instead.
    ))
    addType("Contact_Collection",List(
      Member("add",List("Contact"), "Nothing"), // Adds a contact
      Member("clear","Nothing"), // Clears the collection
      Member("name","String"), // Gets the name of this collection
      Member("set_at",List("Number", "Contact"), "Nothing"), // Sets the contact at index
      Member("set_name",List("String"), "Nothing") // Sets the name of this collection
    ))
    addType("Pictures",List(
      Member("rand","Picture") // Not supported anymore
    ))
    addType("Songs",List(
      Member("rand","Song") // Not supported anymore
    ))
    addType("Sound",List(
      Member("pause","Nothing"), // Not supported anymore
      Member("resume","Nothing"), // Not supported anymore
      Member("state","String"), // Not supported anymore
      Member("stop","Nothing") // Not supported anymore
    ))
    addType("String_Collection",List(
      Member("share",List("String"), "Nothing"), // Shares the content (email, sms, social)
      Member("@reverse",List(), "Nothing") // NOT SURE.. appears often, not in the REF
    ))
    addType("Tile",List(
      Member("panorama","Boolean"), // This property is deprecated.
      Member("set_panorama",List("Boolean"), "Nothing") // Sets if the tile should spread accross the screen.
    ))

}
