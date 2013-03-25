meta version "v2.2,nothing";
meta name "Typewriter";
meta icon "abc";
meta color "#ff007fff";
// Types text into a text box one character at a time.
meta isLibrary "yes";
meta platform "current";

action Typewriter(initialText: String, finalText: String, charsPerSecond: Number) {
  // Slowly writes text to a text box. No purpose, just looks cool.
  $box1 := wall→create_text_box($initialText, 36);
  $box1→post_to_wall;
  foreach character in $finalText→substring($initialText→count, $finalText→count - $initialText→count) where true do {
    $box1→set_text($box1→text ∥ $character);
    time→sleep(1 / $charsPerSecond);
  }
}

