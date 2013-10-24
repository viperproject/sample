meta version "v2.2,js,ctx";
meta name "yahoo weather • Austria";
meta icon "weather";
meta color "#ff007fff";
meta platform "current";
// yahoo weather forecast Austria
// fixed bug.

action open_geoplan_explorer() {
  web→browse("http://isithackday.com/geoplanet-explorer/index.php");
}

action main() {
  wall→clear;
  wall→clear_buttons;
  wall→set_reversed(true);
  wall→set_title("Austria");
  wall→add_button("feature.settings", "C / F");
  wall→add_button("transport.play", "restart");
  wall→set_background(colors→green→darken(0.3)→make_transparent(0.4));
  code→init_Capital_City_Woeids;
  $name, $woeid := code→pic_capital_citys_woeid();
  if not data→initialized or data→at_capital_city_woeids→is_invalid then {
    wall→clear;
    code→init_Capital_City_Woeids;
    data→use_Celsius := wall→ask_boolean("select forecast unit:" ∥ "\nyes: Celsius / no: Farenheit", "Celsius/Fahrenheit");
    data→initialized := true;
  }
  wall→clear;
  wall→set_title($name);
  $deg := "";
  if data→use_Celsius then {
    wall→set_subtitle("Unit: Celsius");
    $deg := "&u=c";
  }
  else {
    wall→set_subtitle("Unit: Fahrenheit");
  }
  $weather\_rss_strg := web→download("http://weather.yahooapis.com/forecastrss?w=" ∥ $woeid ∥ $deg);
  $msgs := web→feed($weather\_rss_strg);
  $msgs→post_to_wall;
}

action init_Capital_City_Woeids() {
  data→at_capital_city_woeids := collections→create_string_map;
  //
  data→at_capital_city_woeids→set_at("Bregenz", "551476");
  data→at_capital_city_woeids→set_at("Graz", "548536");
  data→at_capital_city_woeids→set_at("Innsbruck", "550763");
  data→at_capital_city_woeids→set_at("Linz", "545801");
  data→at_capital_city_woeids→set_at("Salzburg", "547826");
  data→at_capital_city_woeids→set_at("Vienna", "551801");
  data→at_capital_city_woeids→set_at("Villach", "540859");
  data→at_capital_city_woeids→set_at("Eisenstadt", "538800");
  data→at_capital_city_woeids→set_at("St. Pölten", "543662");
  meta private;
}
var at_capital_city_woeids : String_Map {
}

action pic_capital_citys_woeid() returns(name: String, woeid: String) {
  $names := collections→create_string_collection;
  foreach key in data→at_capital_city_woeids→keys
  where true
  do {
    $names→add($key);
  }
  $idx := wall→pick_string("", "", $names);
  $name := $names→at($idx);
  $woeid := data→at_capital_city_woeids→at($name);
  meta private;
}

action init_countries() {
  $Burgenland := "Eisenstadt,Güssing,Jennersdorf,Mattersburg,Neusiedel am See,Oberpullendorf,Oberwart" →split(",");
  $Carinthia := "Feldkirchen,Friesach,Heiligenblut,Hermagor,Klagenfurt,Spital an ser Drau,Villach,Völkermarkt,Wolfsberg" →split("Feldkirchen,Friesach,Heiligenblut,Hermagor,Klagenfurt,Spital an ser Drau,Villach,Völkermarkt,Wolfsberg,");
  $Lower_Austria := "Amstetten,Baden,Bruck an der Leitha,Gänserndorf,Gmünd,Hohenwarth Möhlbach am Manhartsberg,Horn, Klosterneuburg,Korneuburg,Melk,Mistelbach,Mödling,Neunkirchen,Puchberg,Reichenau an der Rax,Retz,Scheibbs,St. Pölten,St.Veith,Stockerau,Tulln an der Donau,Waidhofen an der Thaya,Wiener Neustadt, Zwettl," →split(",");
  $Upper_Austria := "" →split(",");
  $Styria := "Aigen im Ennstal,Bad Gleichenberg,Bad Mitterndorf,Bad Radkersburg,Bruck an der Muhr,Deutsch Landsberg,Feldbach, Fürstenfeld,Gratkorn, Graz,Judenburg,Kapfenberg,Knittelfeld,Lanbach,Leibnitz,Leoben,Mariazell,Murau,Mürzzuschlag,Ramsau am Dachstein,Schladming,Seietsberg,Voitsberg,Weiz" →split(",");
  $Tyrol := "Imst,Innsbruck,Ischgl,Jungholz,Kitzbühel,Landeg,Lienz,Matrei,Reutte,Schwaz,St. Christoph,St. Johann in Tirol,Steinbach am Brenner" →split(",");
  $Salzburg := "Salzburg," →split(",");
  $Vienna := "Vienna," →split(",");
  $Vorarlberg := "Bludenz,Bregenz,Dornbirn,Feldkirch,Hirschberg,Kirche,Klosterle,Lech,Mittelberg,Zürs" →split(",");
  meta private;
}
var initialized : Boolean {
}
var use_Celsius : Boolean {
}

event tap_wall_Page_Button(item: Page_Button) {
  if $item→icon→equals("feature.settings") then {
    if data→use_Celsius then {
      data→use_Celsius := false;
      wall→set_subtitle("F a h r e n h e i t");
    }
    else {
      data→use_Celsius := true;
      wall→set_subtitle("C e l s i u s");
    }
  }
  else {
    code→main;
  }
  meta private;
}

action reset() {
  data→initialized := false;
}