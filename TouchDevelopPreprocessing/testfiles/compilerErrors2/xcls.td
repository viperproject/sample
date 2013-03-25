meta version "v2.2,nothing";
meta name "latin serif";
meta icon "flowchart";
meta color "#ff008000";
// Use the hidden serif font for simple Latin characters.
meta isLibrary "yes";
meta platform "current";

action main() {
  code→from("QWERTYUIOPASDFGHJKLZXCVBNM\nqwertyuiopasdfghjklzxcvbnm\n1234567890\n @#$%&()-•\\!;:\'\"?/,\n^[]{}<>*-+=_~|.")→post_to_wall;
  code→to("ＱＷＥＲＴＹＵＩＯＰＡＳＤＦＧＨＪＫＬＺＸＣＶＢＮＭ\nｑｗｅｒｔｙｕｉｏｐａｓｄｆｇｈｊｋｌｚｘｃｖｂｎｍ\n１２３４５６７８９０\n ＠＃＄％＆（）－ἂ＼！；：＇＂？／，\n＾［］｛｝＜＞＊－＋＝＿～｜．")→post_to_wall;
}

action from(sans: String) returns serif: String {
  $strings := collections→create_string_collection;
  foreach s in $sans where true do {
    $x := $s→to_unicode;
    if $s→equals("\n") or $s→equals(" ") then {
      $strings→add($s);
    }
    else {
      $strings→add((($x - 288))→to_character);
    }
  }
  $serif := $strings→join("");
}

action to(serif: String) returns sans: String {
  $strings := collections→create_string_collection;
  foreach s in $serif where true do {
    $x := $s→to_unicode;
    if $x = 10 then {
      $strings→add("\n");
    }
    else {
      if $x > 65248 then {
        $strings→add(($x - 65248)→to_character);
      }
      else {
        $strings→add($s);
      }
    }
  }
  $sans := $strings→join("");
}

