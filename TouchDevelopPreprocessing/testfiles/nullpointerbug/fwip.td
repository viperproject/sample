meta version "v2.2,nothing";
meta name "survey";
//
meta platform "current";

action main() {
  $row := records→player1_table→add_row;
  $b1 := wall→ask_boolean("Say yes if you are Male", "Say no if you are Female");
  $b := wall→ask_boolean("Are you senior", "Not");
  $row→grade→set($b);
  if $b→equals(false) then {
    skip;
  }
  else {
    wall→ask_number("How many math classes did you completed?");
    wall→ask_string("Class of 2013?");
  }
  $s := wall→ask_string("What math class do you take?");
  $row→name→set($s);
  $x := wall→ask_number("How many classes are you currently taking?");
  $row→vote→set($x);
  $x1 := wall→ask_number("How long you been in Rainier Beach H.S");
  "Well done, Thank you";
  $t := time→now;
  $row→time→set($t);
  $place := senses→current_location;
  $row→place→set($place);
  foreach p1 in records→player1_table where true do {
    $s1 := $p1→name→get;
    $vote := $p1→grade→get;
    $dt := $p1→time→get;
    $loc := $p1→place→get;
    $p1→post_to_wall;
  }
  $sum := 0;
  foreach p in records→player1_table where true do {
    $vote1 := $row→vote→get;
    if $b then {
      $sum := $sum + $vote1;
    }
  }
  $avg := $sum / records→player1_table→count;
  $avg := $avg / records→player1_table→count;
  $avg→post_to_wall;
}
table player1 {
  type = "Table";
  fields {
    name: String_field
    vote: Number_field
    place: Location_field
    time: DateTime_field
    grade: Boolean_field
  }
}

