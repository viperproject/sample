meta version "v2.2,nothing";
meta name "morse code";
meta icon "FlipChart";
meta color "#ff800080";
//
meta platform "current";

action main() {
  "Vitejte ve scriptu morseovka!" →post_to_wall;
  time→sleep(0.3);
  $sentence := wall→ask_string("Zadejte pozadovanou vetu");
  $repeat := wall→ask_number("Zadejte pocet opakovani");
  $sentence := code→ScTranslate($sentence);
  $bord := media→create_board(640);
  $bord→post_to_wall;
  for 0 ≤ i < $repeat do {
    foreach x in $sentence where true do {
      code→ScSignal($x, $bord);
      $bord→set_background(colors→black);
      $bord→update_on_wall;
      time→sleep(0.5);
    }
  }
  $sentence := "Odvysilano : " ∥ $sentence;
  $sentence→post_to_wall;
}

action ScTranslate(s0: String) returns result: String {
  $output := $s0→to_upper_case;
  $output := $output→replace("A", ".-!");
  $output := $output→replace("B", "-...!");
  $output := $output→replace("C", "-.-.!");
  $output := $output→replace("D", "-..!");
  $output := $output→replace("E", ".!");
  $output := $output→replace("F", "..-.!");
  $output := $output→replace("G", "--.!");
  $output := $output→replace("H", "....!");
  $output := $output→replace("I", "..!");
  $output := $output→replace("J", ".---!");
  $output := $output→replace("K", "-.-!");
  $output := $output→replace("L", ".-..!");
  $output := $output→replace("M", "--!");
  $output := $output→replace("N", ".-.!");
  $output := $output→replace("O", "---!");
  $output := $output→replace("P", ".--.!");
  $output := $output→replace("Q", "--.-!");
  $output := $output→replace("R", ".--.!");
  $output := $output→replace("S", "...!");
  $output := $output→replace("T", "-!");
  $output := $output→replace("U", "..-!");
  $output := $output→replace("V", "...-!");
  $output := $output→replace("W", ".--!");
  $output := $output→replace("X", "-..-!");
  $output := $output→replace("Y", "-.--!");
  $output→replace("Z", "--..!");
  $result := $output;
}

action ScSignal(s1: String, board0: Board) {
  if $s1→equals("-") then {
    $board0→set_background(colors→white);
    $board0→update_on_wall;
    phone→vibrate(0.08);
    time→sleep(0.08);
  }
  if $s1→equals(".") then {
    $board0→set_background(colors→white);
    $board0→update_on_wall;
    phone→vibrate(0.25);
    time→sleep(0.4);
  }
}

