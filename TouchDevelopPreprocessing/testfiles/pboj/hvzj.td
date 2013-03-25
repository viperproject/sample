meta version "v2.2,nothing";
meta name "text to my voice";
meta icon "businessperson";
meta color "#ff007fff";
// Converts text to your own voice!!!
meta platform "current";

action main() {
  // Converts text to speech and asks the user to say the unknown words.
  $words := code→ask_sentence;
  code→record_missing_words($words);
  code→play_words($words);
}
table word_sounds {
  type = "Index";
  keys {
    word: String
  }
  fields {
    sound: Sound_field
  }
}

action record_missing_words(words: String_Collection) {
  // Scans the words and record missing ones.
  foreach word in $words where records→word_sounds_index→at($word)→sound→get→is_invalid do {
    ("recording " ∥ $word)→post_to_wall;
    wall→prompt("Don\'t know how to say \"" ∥ $word ∥ "\", ready to record it?");
    $sound := senses→record_microphone;
    records→word_sounds_index→at($word)→sound→set($sound);
  }
  meta private;
}

action play_words(words: String_Collection) {
  // Play each word one by one
  foreach word in $words where true do {
    $w := records→word_sounds_index→at($word)→sound→get;
    $w→play;
    time→sleep($w→duration);
  }
  meta private;
}

action ask_sentence() returns words: String_Collection {
  // Get the list of words to say...
  $sentence := wall→ask_string("What do you want to say?");
  $words := $sentence→to_lower_case→split(" ");
  meta private;
}

