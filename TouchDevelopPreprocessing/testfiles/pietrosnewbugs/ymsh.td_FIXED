meta version "v2.2,nothing";
meta name "Text-n-Wall";
meta icon "runningman";
meta color "#ff00008b";
// Text and walk safely thanks to the live camera video.
meta platform "current";

action main() {
  // Displays the live camera while entering a text message.
  senses→camera→post_to_wall;
  social→send_sms(phone→choose_address→name, wall→ask_string(""));
}

