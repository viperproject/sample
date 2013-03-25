meta version "v2.2,nothing";
meta name "How Common is my Name?";
meta icon "ABC";
//
meta platform "current";

action main() {
  $strings := collections→create_string_collection;
  $strings→add("First Name");
  $strings→add("Last Name");
  $index := wall→pick_string("Name Type", "Choose one. ", $strings);
  $strings→set_at(0, "Male");
  $strings→set_at(1, "Female");
  $index2 := wall→pick_string("Gender", "Choose only one.", $strings);
  $name := wall→ask_string("Enter the name you want to search: ");
  code→fetch($index, $index2, $name);
}

action fetch(nameType: Number, genderType: Number, name: String) {
  // //0 s first name and male
  $x3 := "http://www.namestatistics.com/search.php?name=&type=&gender=";
  $insertName := $x3→index_of("name=", 0) + 5;
  $withName := $x3→insert($insertName, $name);
  $x2 := "";
  if $nameType = 0 then {
    $x2 := "first";
  }
  else {
    $x2 := "last";
  }
  $x := $withName→index_of("type=", 0) + 5;
  $withType := $withName→insert($x, $x2);
  $x4 := $withType→index_of("gender=", 0) + 7;
  $s := "";
  if $genderType = 0 then {
    $s := "male";
  }
  else {
    $s := "female";
    skip;
  }
  $x5 := $withType→insert($x4, $s);
  $urlString := $x5→to_lower_case;
  skip;
  if not web→is_connected then {
    "Sorry, you don\'t have an internet connection!" →post_to_wall;
  }
  $fullSource := web→download($urlString);
  $start := $fullSource→index_of("<h4>Around <b>", 0);
  $s2 := $fullSource→substring($start + 14, 10);
  $counter := 0;
  $curIndex := 0;
  while $curIndex < $s2→count do {
    if code→isDigit($s2→at($curIndex)) then {
      $counter := $counter + 1;
    }
    $curIndex := $curIndex + 1;
  }
  $number := $s2→substring(0, $counter);
  if $start = - 1 then {
    "Your name is very unique! Very few people have this name." →post_to_wall;
  }
  else {
    ($number ∥ " people have your name!")→post_to_wall;
  }
  meta private;
}

action isDigit(s: String) returns b: Boolean {
  if $s→equals("0") or $s→equals("1") or $s→equals("2") or $s→equals("3") or $s→equals("4") or $s→equals("5") or $s→equals("6") or $s→equals("7") or $s→equals("8") or $s→equals("9") then {
    $b := true;
  }
  else {
    $b := false;
  }
  meta private;
}

