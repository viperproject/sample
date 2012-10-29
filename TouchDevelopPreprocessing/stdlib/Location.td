
// Gets the altitude of the coordinate
var altitude : Number {}
// Gets the course of the coordinate
var course : Number {}
// Gets the speed of the coordinate
var speed : Number {}
// Gets the vertical accuracy of the coordinate
var vert_accuracy : Number {}
// Gets the horizontal accuracy of the coordinate
var hor_accuracy : Number {}
// Gets the latitude of the coordinate
var latitude : Number {}
// Gets the longitude of the coordinate
var longitude : Number {}
// Returns true if the current instance is useless
var is_invalid : Boolean {}

// Calculates the distance in meters
action distance(other : Location) : ret:Number {

}

// Indicates if this instance is equal to the other
action equals(other : Location) : ret:Boolean {
    assert(not is_invalid)
    assert(not other.is_invalid)
    ret := (altitude == other.altitude) && (course == other.course) && (speed == other.speed)
        && (vert_accuracy == other.vert_accuracy) && (hor_accuracy == other.hor_accuracy)
        && (latitude == other.latitude) && (longitude == other.longitude)
}


// Displays the location in a map using Bing.
action post_to_wall() {
    assert(not is_invalid);

}

// Shares the location (email, sms, facebook, social or '' to pick from a list)
//action share(network : String, message : String) {
// NATIVE METHOD
//}

// Converts to a string lat,long
action to_string() returns ret:String {
    assert(not is_invalid);
    ret := latitude || "," || longitude;
}


