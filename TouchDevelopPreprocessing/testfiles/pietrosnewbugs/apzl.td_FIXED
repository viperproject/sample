meta version "v2.2,nothing";
meta name "conf app";
//
meta platform "current";

action main() {
  code→read_data;
  code→show;
}
table session {
  type = "Table";
  fields {
    title: String_field
    speakers: String_field
    abstract: String_field
  }
}

action read_data() {
  if records→session_table→count = 0 then {
    $s := web→download("http://db.grinnell.edu/sigcse/sigcse2013/Program/programAppSpreadsheet.asp");
    $strings := $s→split("\n");
    // "Date","Start Time", "End Time", "Location", "Session Type", "Title", "Abstract", "Speaker Name(s)"
    $strings→remove_at(0);
    records→session_table→clear;
    for 0 ≤ i < 10 do {
      $line := $strings→at($i);
      $cells := $line→trim("\"")→split("\",\"");
      $session1 := records→session_table→add_row;
      $session1→title→set($cells→at(5));
      $session1→abstract→set($cells→at(6));
      $session1→speakers→set($cells→at(7));
    }
  }
  meta private;
}

action show() {
  if box→is_init then {
    skip;
  }
  if true then {
    foreach e in records→session_table where true do {
      do box {
        box→set_margins(1, 1, 1, 1);
        box→set_padding(0.5, 0.5, 0.5, 0.5);
        box→set_background(colors→random);
        box→on_tapped($handler)
        where handler() {
          code→show_session($e);
        }
        do box {
          box→set_foreground(colors→white);
          box→set_text_wrapping(true, 15);
          box→set_font_size(2.5);
          $e→title→get→post_to_wall;
        }
        do box {
          $e→speakers→get→post_to_wall;
          box→set_text_wrapping(true, 15);
          box→set_foreground(colors→white);
        }
      }
    }
  }
}

action show_session(session1: session) {
  if box→is_init then {
    wall→set_title($session1→title→get);
    wall→set_subtitle($session1→speakers→get);
  }
  if true then {
    do box {
      $session1→abstract→get→post_to_wall;
      box→set_text_wrapping(true, 15);
    }
  }
}

