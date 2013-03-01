meta version "v2.2,nothing";
meta name "one bus away services";
meta icon "DeliveryTruck";
// A library that allows to query the OneBusAway API services. OneBusAway provides real time data about various transportation agencies in the Puget Sound, WA.
// The full documentation of this service is available at http://developer.onebusaway.org/modules/onebusaway-application-modules/current/api/where/index.html .
meta isLibrary "yes";
meta platform "current";

action example() {
  // This library let's you build transportation apps in the Puget Sound area thanks to the OneBusAway services.
  // ## Initialization
  // You need to register at OneBusAway and request an API key in order to use this script. The API key needs to be passed to the `initialize` action.
  if data→api_key→is_empty then {
    $apikey := wall→ask_string("Please enter your OneBusAway API key to run this demo. You can request the API key at http://www.onebusaway.org/p/OneBusAwayApiService.action");
    code→initialize($apikey);
  }
  // ## Stops
  // Let's start by finding the stops that are nearby.
  "Nearby stops:" →post_to_wall;
  $stops := code→stops_nearby(senses→current_location_accurate);
  $stops→post_to_wall;
  // ## Arrivals and departures
  // Once you have identified a stop, you can query the estimated arrival times of the routes on that stop.
  $first_stop := $stops→at(0);
  ("Arrivals for " ∥ $first_stop→message)→post_to_wall;
  $arrivals := code→arrivals_for_stop($first_stop→from);
  $arrivals→post_to_wall;
}

action initialize(api_key: String) {
  // Stores the OneBusAway API Key. You need to request an API Key from http://www.onebusaway.org/p/OneBusAwayApiService.action .
  data→api_key := $api_key;
}
var api_key : String {
}

action query(method: String, args: String_Map) returns r: Json_Object {
  $url := "http://api.onebusaway.org/api/" ∥ $method ∥ ".json?key=" ∥ web→url_encode(data→api_key);
  foreach key in $args→keys where true do {
    $url := $url ∥ "&" ∥ $key ∥ "=" ∥ web→url_encode($args→at($key));
  }
  $request := web→create_request($url);
  $response := $request→send;
  $r := $response→content_as_json;
  meta private;
}

action stops_nearby(loc: Location) returns stops: Message_Collection {
  // Queries for stops nearby the given location. The messages returned contain the information about each stop:
  // * the `from` contains stop `id`,
  // * the `message` contains stop `name`,
  // * the `values` map contains `route id` and `route description` pairs.
  $method := "where/stops-for-location";
  $args := collections→create_string_map;
  $args→set_at("lon", $loc→longitude→to_string);
  $args→set_at("lat", $loc→latitude→to_string);
  $args→set_at("radius", "250");
  $r := code→query($method, $args);
  $stops := collections→create_message_collection;
  if not $r→is_invalid then {
    foreach stopjs in $r→field("data")→field("stops") where true do {
      $stop := social→create_message($stopjs→string("name"));
      $stop_loc := locations→create_location($stopjs→number("lat"), $stopjs→number("lon"));
      $stop→set_location($stop_loc);
      $stop→set_from($stopjs→string("id"));
      foreach routejs in $stopjs→field("routes") where true do {
        $stop→values→set_at($routejs→string("id"), $routejs→string("description"));
      }
      $stops→add($stop);
    }
  }
}

action arrivals_for_stop(stop: String) returns arrivals: Message_Collection {
  // Queries for stops nearby the given location. The messages returned contain the information about each stop:
  // * the `title` contains stop `id`,
  // * the `message` contains stop `name`,
  // * the `values` map contains `route id` and `route description` pairs.
  $method := "where/arrivals-and-departures-for-stop/" ∥ web→url_decode($stop);
  $args := collections→create_string_map;
  $r := code→query($method, $args);
  $arrivals := collections→create_message_collection;
  if not $r→is_invalid then {
    $datajs := $r→field("data");
    $adsjs := $datajs→field("arrivalsAndDepartures");
    foreach adjs in $adsjs where true do {
      $ad := social→create_message($adjs→string("tripHeadsign"));
      $ad→set_message($adjs→string("tripId"));
      $ad→set_from($adjs→string("routeShortName"));
      $ad→set_time($adjs→time("predictedArrivalTime"));
      $arrivals→add($ad);
    }
  }
}

