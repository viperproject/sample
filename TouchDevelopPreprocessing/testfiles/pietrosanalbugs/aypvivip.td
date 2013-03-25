meta version "v2.2,nothing";
meta name "test OSM";
meta icon "Bookmark";
//
meta platform "location,maps,search,translation";

action main() {
  $request := web→create_request("http://polytech.lp-metinet.com/www/PFE_Web/images/map.osm");
  $response := $request→send;
  $nodesXML := $response→content_as_xml→children("node");
  $PolygXML := $response→content_as_xml→children("way");
  $boundsXML := $response→content_as_xml→child("bounds");
  $medlat := ($boundsXML→attr("minlat")→to_number + $boundsXML→attr("maxlat")→to_number) / 2;
  $medlon := ($boundsXML→attr("minlon")→to_number + $boundsXML→attr("maxlon")→to_number) / 2;
  $map := maps→create_map;
  foreach e in $nodesXML where true do {
    $id := $e→attr("id")→to_number;
    $node1 := records→node_index→at($id);
    $node1→lat→set($e→attr("lat")→to_number);
    $node1→lon→set($e→attr("lon")→to_number);
  }
  records→node_index→count→post_to_wall;
  foreach e5 in $PolygXML where true do {
    $tagXML := $e5→children("tag");
    foreach e3 in $tagXML where true do {
      if $e3→attr("k")→equals("building") then {
        $polygone := collections→create_location_collection;
        $refNodeXML := $e5→children("nd");
        foreach e4 in $refNodeXML where true do {
          $nd := $e4→attr("ref")→to_number;
          $lat := records→node_index→at($nd)→lat→get;
          $lon := records→node_index→at($nd)→lon→get;
          $polygone→add(locations→create_location($lat, $lon));
          $map→fill_region($polygone, colors→gray, colors→black, 1);
        }
      }
    }
  }
  $map→post_to_wall;
  $map→set_zoom(18);
  $map→set_center(locations→create_location($medlat, $medlon));
}
table node {
  type = "Index";
  keys {
    id: Number
  }
  fields {
    lat: Number_field
    lon: Number_field
  }
}
var pic : Picture {
  is\_resource = true;
}

