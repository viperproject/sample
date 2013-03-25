meta version "v2.2,nothing";
meta name "Truth or Dare";
meta icon "cherry";
meta color "#ffff2800";
// This is a simple truth or dare game with questions meant for adults.
meta platform "current";

action main() {
  $x := wall→ask_boolean("Play the game ?", "")→to_number;
  wall→clear;
  if $x = 1 then {
    code→play;
  }
  skip;
}

action play() {
  wall→set_background(colors→random→make_transparent(0));
  $mTruth := collections→create_string_collection;
  $mTruth→add_many("What is your fantasy?;Do you like black or white women?;What do you like in women?;Do you masturbate?;What is the dumbest thing you have ever done?;Would you have sex with two women?;What would you like a woman do to you besides sex or a bj?;When did you last have sex?;How long can you have sex?;Do you like children?;What turns you on the most?;Would you kiss the person next to you?;What attracts you to a person of the opposite sex?;Have you got a secret talent?;What is the wildest thing you have ever done?;What is your worst fear?;Have you ever had a crush and who was it?;If you had 24 hours to live what would you do?;What is the meanest thing you have ever done?;Do you shower everyday?; If you took a shit and there was no paper what would you do?;What do you regret?;What celebrity do you have a crush or fantasized on?;What is the freakiest thing you have ever done?;What is the weirdest place you have ever done it or wish to do it?;Have you ever had a one night stand?;Have you ever watched porn and name three freaky things you have ever seen!;What is your favorite position?;Does size matter? Breasts in women and Dicks in men!;Would you have a threesome?" →split(";"));
  $mDare := collections→create_string_collection;
  $mDare→add_many("Blow a kiss to the nearest woman in your vicinity.;Make a dance move.;Sing a song.;Kiss a member of the opposite sex on the cheek.;Give someone in the room a lap dance.;Make a prank call to anybody on your phone.;Have a one minute conversation with a chair or a wall.;Go outside and howl like a wolf three times.;Call a hotel and ask if the serve dinosaurs.;Perform an act that you consider a talent.;Imitate someone next to you or someone you know.;Update your status on Facebook that you have diarrhea and leave for two minutes.;Make an angry look for 30 seconds." →split(";"));
  $fTruth := collections→create_string_collection;
  $fTruth→add_many("What is your fantasy?;Do you like black or white women?;What do you like in women?;Do you masturbate?;What is the dumbest thing you have ever done?;Would you have sex with two women?;What would you like a woman do to you besides sex or a bj?;When did you last have sex?;How long can you have sex?;Do you like children?;What turns you on the most?;Would you kiss the person next to you?;What attracts you to a person of the opposite sex?;Have you got a secret talent?;What is the wildest thing you have ever done?;What is your worst fear?;Have you ever had a crush and who was it?;If you had 24 hours to live what would you do?;What is the meanest thing you have ever done?;Do you shower everyday?; If you took a shit and there was no paper what would you do?;What do you regret?;What celebrity do you have a crush or fantasized on?;What is the freakiest thing you have ever done?;What is the weirdest place you have ever done it or wish to do it?;Have you ever had a one night stand?;Have you ever watched porn and name three freaky things you have ever seen!;What is your favorite position?;Does size matter? Breasts in women and Dicks in men!;Would you have a threesome?" →split(";"));
  $fDare := collections→create_string_collection;
  $fDare→add_many("Blow a kiss to the nearest woman in your vicinity.;Make a dance move.;Sing a song.;Kiss a member of the opposite sex on the cheek.;Give someone in the room a lap dance.;Make a prank call to anybody on your phone.;Have a one minute conversation with a chair or a wall.;Go outside and howl like a wolf three times.;Call a hotel and ask if the serve dinosaurs.;Perform an act that you consider a talent.;Imitate someone next to you or someone you know.;Update your status on Facebook that you have diarrhea and leave for two minutes.;Make an angry look for 30 seconds." →split(";"));
  $s := wall→ask_boolean("Are you Male?", "Click Yes for Male or No for Female");
  wall→clear;
  if $s→equals(true) then {
    art→maleIcon2→post_to_wall;
    $trudar := wall→ask_boolean("Do you want to answer the Truth ?", "Click Yes for Truth or No for Dare");
    wall→clear;
    if $trudar→equals(true) then {
      art→maleIcon2→post_to_wall;
      $mTruth→random→post_to_wall;
      $x1 := 1;
      code→main;
    }
    else {
      art→maleIcon2→post_to_wall;
      $mDare→random→post_to_wall;
      code→main;
    }
  }
  else {
    if $s→equals(false) then {
      art→femaleIcon2→post_to_wall;
      $trudar1 := wall→ask_boolean("Do you want to answer the Truth ?", "Click Yes for Truth or No for Dare");
      wall→clear;
      if $trudar1→equals(true) then {
        art→femaleIcon2→post_to_wall;
        $fTruth→random→post_to_wall;
        code→main;
      }
      else {
        art→femaleIcon2→post_to_wall;
        $fDare→random→post_to_wall;
        code→main;
      }
    }
  }
  ... ;
  meta private;
}
var maleIcon2 : Picture {
  is\_resource = true;
  url = "http://m.bing.com/search/Thumbnail.aspx?c=nntRxexn&w=320&h=480&i=http%3A%2F%2Fwww.veryicon.com%2Ficon%2Fpng%2FSystem%2FToolbar%2FMale.png";
}
var c : Color {
  is\_resource = true;
  url = "#ff3e64bc";
}
var femaleIcon2 : Picture {
  is\_resource = true;
  url = "http://m.bing.com/search/Thumbnail.aspx?c=dDDvbFaW&w=320&h=480&i=http%3A%2F%2Fwww.mricons.com%2Fstore%2Fpng%2F5206_1672_128_user_male_female_icon.png";
}
var TruthOrDare : Picture {
  is\_resource = true;
  url = "http://m.bing.com/search/Thumbnail.aspx?c=LPoxyStd&w=320&h=480&i=http%3A%2F%2Fus.123rf.com%2F400wm%2F400%2F400%2F3dclipartsde%2F3dclipartsde1008%2F3dclipartsde100800081%2F7569711-3d-male-icon-toon-characters-have-a-discussion.jpg";
}

