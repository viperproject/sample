meta version "v2.2,js,ctx";
meta name "social stuff";
meta icon "AddUser";
meta color "#ff00008b";
meta platform "current";
// Demonstrates interaction with social networks

action search_facebook_friends(term: String) {
  // Searches your facebook friends
  $term := $term→to_lower_case;
  $contacts := social→contacts("facebook");
  foreach contact in $contacts
  where true
  do {
    if $contact→name→to_lower_case→contains($term) then {
      $contact→post_to_wall;
    }
  }
}

action search_social_networks(search_terms: String) {
  // Searches for tweets and facebook posts.
  $messages := social→search("twitter", $search_terms);
  // messages contains a list of 'social' message. a message could represent a Twitter tweet, Facebook post, rss entry, etc...
  $messages→add_many(social→search("facebook", $search_terms));
  $messages→sort_by_date;
  // messages sorted by time so that the newest messages show up on top of the wall
  $messages→post_to_wall;
}

action search_TouchDevelop() {
  // Search tweets and facebook posts about TouchDevelop.
  code→search_social_networks("TouchDevelop");
}

action search_places() {
  // Searches for nearby Facebook places
  $loc := senses→current_location;
  // loc contains our current location
  $places := social→search_places_nearby("facebook", "", $loc, 1000.0);
  // places contains all the Facebook places within a kilometer radius
  $places→post_to_wall;
}

action quick_share() {
  // Quick and easy way to share messages on social networks
  "Hello from TouchDevelop!" →share("");
  // since we did not provide the name of a particular social network, TouchDevelop will display a list
}

action snap_to_facebook() {
  // Takes a picture and posts it to Facebook.
  $pic := senses→take_camera_picture;
  if $pic→is_invalid then {
    // the user might have canceled the camera capture, so it is always good to check if pic is valid
    time→stop;
    // we stopped executing since there was no picture
  }
  $loc := senses→current_location;
  $address := locations→describe_location($loc);
  // address contains the civic address at the current location. this is commonly called 'reverse geocoding'.
  $pic→share("facebook", "Checking out " ∥ $address);
  // the funny || character glues the two string together. You've just posted a picture on Facebook, congratulations!
}