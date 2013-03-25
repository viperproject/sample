meta version "v2.2,nothing";
meta name "Convert letters";
//
meta platform "current";

action main() {
  $CharLookup := "F<DULT:PBQRKVYJGHCNEA{WXIO}SM\">Zf,dult;pbqrkvyjghcnea[wxio]sm\'.z";
  $s := wall→ask_string("");
  $result := "";
  foreach char in $s where true do {
    $charCode := $char→to_unicode;
    $i := $charCode - "А" →to_unicode;
    if $i ≥ 0 and $i < $CharLookup→count then {
      $result := $result ∥ $CharLookup→at($i);
    }
    else {
      $result := $result ∥ $char;
    }
  }
  $result→copy_to_clipboard;
  $result→concat(" was copied to clipboard.")→post_to_wall;
}
var sound : Sound {
  is\_resource = true;
}

