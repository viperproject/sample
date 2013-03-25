meta version "v2.2,nothing";
meta name "Sample Quiz App";
meta icon "award";
//
meta platform "current";

action main() {
  data→pgqstn := 0;
  wall→create_text_box("START KWIZ", 40)→post_to_wall;
  wall→create_text_box("Add New Questions", 40)→post_to_wall;
}
table kwiz {
  type = "Table";
  fields {
    options: String_field
    tried: Boolean_field
    qstnum: Number_field
    qstn: String_field
  }
}
var pgqstn : Number {
}

action qstn(qnum: Number) returns qstn: String {
  $qstn := "";
  if records→kwiz_table→count > 0 then {
    foreach k in records→kwiz_table where true do {
      if $k→qstnum→get→to_string→equals($qnum→to_string) then {
        $qstn := $k→qstn→get;
        data→strings2 := $k→options→get→split(";");
      }
    }
  }
  meta private;
}

event tap_wall_TextBox(item: TextBox) {
  if $item→text→equals("Next Q") or $item→text→equals("START KWIZ") then {
    code→NextQstn;
  }
  else {
    if $item→text→equals("Add New Questions") then {
      $nwqst := wall→ask_string("New Question:");
      $nwqst→post_to_wall;
      $optn := wall→ask_string("Options:(eg. A;B;C)");
      $optn→post_to_wall;
      code→addqstn($nwqst, $optn);
    }
  }
  meta private;
}
var s : String {
}

action NextQstn() {
  if records→kwiz_table→count > 0 then {
    data→pgqstn := data→pgqstn + 1;
    wall→push_new_page;
    if data→pgqstn < records→kwiz_table→count then {
      wall→create_text_box("Next Q", 40)→post_to_wall;
    }
    $s := code→qstn(data→pgqstn);
    $x := wall→pick_string($s, "", data→strings2);
    $x→post_to_wall;
  }
  meta private;
}

action addqstn(nwqstn: String, nwqstoptn: String) {
  $k := records→kwiz_table→add_row;
  $k→qstnum→set((records→kwiz_table→count) + 1);
  $k→qstn→set($nwqstn);
  $k→options→set($nwqstoptn);
  $k→tried→set(false);
  meta private;
}
var strings2 : String_Collection {
}

