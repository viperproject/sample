meta version "v2.2,nothing";
meta name "Facebook vote";
//
meta platform "current";

action main() {
  $row := records→Survey_table→add_row;
  $s := wall→ask_string("Please type your name");
  $row→name→set($s);
  $xau_woac := time→now;
  $row→time→set($xau_woac);
  $a := wall→ask_number("Enter you age");
  $row→age→set($a);
  $g := wall→ask_string("Enter your gender");
  $row→gender→set($g);
  $x := wall→ask_number("Enter your grade");
  $row→grade→set($x);
  $x1 := wall→ask_number("How many hours you use to study");
  $row→hour_study→set($x1);
  $x2 := wall→ask_number("How many hours you use Facebook");
  $row→hour_facebook→set($x2);
  $x3 := wall→ask_number("How many hours you sleep");
  $row→hour_sleep→set($x3);
  $b := wall→ask_boolean("Vote for Facebook", "");
  $row→vote→set($b);
  records→Survey_table→count;
  records→Survey_table→post_to_wall;
  $fb_vote := 0;
  foreach s1 in records→Survey_table where true do {
    $b1 := $s1→vote→get;
    if $b1 then {
      $fb_vote := $fb_vote + 1;
    }
  }
  $x4 := $fb_vote / records→Survey_table→count;
  $x4→post_to_wall;
}
table Survey {
  type = "Table";
  fields {
    name: String_field
    age: Number_field
    address: String_field
    time: DateTime_field
    gender: String_field
    grade: Number_field
    hour_study: Number_field
    hour_facebook: Number_field
    hour_sleep: Number_field
    vote: Boolean_field
  }
}

