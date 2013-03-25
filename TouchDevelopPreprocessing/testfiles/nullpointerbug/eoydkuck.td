meta version "v2.2,nothing";
meta name "hw survey";
meta icon "alignleft";
//
meta platform "current";

action main() {
  $row := records→vote_table→add_row;
  $boolean := wall→ask_boolean("YES, I am a student", "NO, I am not a student");
  $row→grade→set($boolean);
  if $boolean→equals(false) then {
    skip;
  }
  else {
    wall→ask_number("What grade are you? ");
  }
  $s := wall→ask_string("What is your name?");
  $row→name→set($s);
  records→vote_table→count;
  $sum := 0;
  foreach v in records→vote_table where true do {
    $b := $v→grade→get;
  }
  if $boolean then {
    $sum := $sum + 1;
  }
  $loc := senses→current_location;
  $dt := time→now;
  $row→time→set($dt);
  records→vote_table→post_to_wall;
}
table vote {
  type = "Table";
  fields {
    name: String_field
    grade: Boolean_field
    time: DateTime_field
    loc: Location_field
    class: Number_field
  }
}

