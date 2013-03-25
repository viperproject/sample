meta version "v2.2,nothing";
meta name "Typewriter";
meta icon "abc";
meta color "#ff007fff";
// Types text into a text box one character at a time.
meta isLibrary "yes";
meta platform "current";

action Example() {
  // Sample usage.
  $initial_text := "This is an example of ";
  $text_to_add := "how typewriter works.";
  code→Typewriter($initial_text, $text_to_add, 3);
}

action Typewriter(initialText: String, textToAdd: String, charsPerSecond: Number) {
  // Types text into a text box at a specified rate.
  $box1 := wall→create_text_box($initialText, 36);
  $box1→post_to_wall;
  foreach character in $textToAdd where true do {
    $box1→set_text($box1→text ∥ $character);
    time→sleep(1 / $charsPerSecond);
  }
}

