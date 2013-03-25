meta version "v2.2,nothing";
meta name "homework";
//
meta platform "current";

action main() {
  records→survey_table→clear;
  $y := records→survey_table→add_row;
  $question := wall→ask_boolean("Is it true that whales can only live in water?", "");
  $y→b→set($question);
  $x := wall→ask_number("How old are you?");
  $y→age→set($x);
  $s := wall→ask_string("What is your name ?");
  $y→name→set($s);
  $dt := time→now;
  $y→time→set($dt);
  $loc := senses→current_location;
  $loc→post_to_wall;
  foreach s1 in records→survey_table where true do {
    $s1→post_to_wall;
  }
}
table survey {
  type = "Table";
  fields {
    name: String_field
    b: Boolean_field
    time: DateTime_field
    age: Number_field
  }
}
table thing {
  type = "Object";
}

