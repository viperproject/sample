meta version "v2.2,nothing";
meta name "bubble popper";
meta icon "controller";
meta color "#ff007fff";
// A library to create bubble popper games
meta isLibrary "yes";
meta platform "current";

action Smile\u0021() {
  $board := media→create_portrait_board(500, 800);
  $board→set_gravity(5, 15);
  $sprite := $board→create_text(75, 20, 40, "Don\'t Pop Red");
  $board→post_to_wall;
  $board→create_boundary(500);
  $board→set_background(colors→random→darken(10));
  wall→set_title("My Bubbles!!");
  // ## Initialization
  // Start by calling the initialize action on the board.
  code→initialize($board);
  // ## Bubbles
  // Call add bubble to register sprites as bubbles that need to be tapped.
  $bubble := $board→create_ellipse(100, 100);
  $bubble→set_color(colors→green);
  code→add_bubble($bubble);
  $bubble := $board→create_ellipse(100, 100);
  $bubble→set_color(colors→blue);
  code→add_bubble($bubble);
  colors→foreground→darken(0.6);
  // ## Bad bubbles
  // Call add bad bubble to register bubbles that should not be tapped.
  $bad_bubble := $board→create_ellipse(80, 80);
  $bad_bubble→set_color(colors→red);
  code→add_bad_bubble($bad_bubble);
  $bad_bubble := $board→create_ellipse(50, 50);
  $bad_bubble→set_color(colors→red);
  code→add_bad_bubble($bad_bubble);
  // ## Physics
  // You can tweak the speed at which bubbles fall down the screen by using the set physics action.
  code→set_physics(250, 50);
  // You can also set friction on individual bubbles to tweak their speed.
  $bad_bubble→set_friction(0.001);
  $bubble→set_acceleration(6, 7);
  // ## Sounds
  data→\_miss_count→post_to_wall;
  data→\_bad_pop_count→post_to_wall;
  data→pops→post_to_wall;
  // ## Scores
  // The library keeps a count of popped bubbles, bad popped bubbles and missed bubbles. Use score to retrieve those values.
  data→pops, data→bad_pop_count, data→miss_count := code→scores();
  $pop_count, $bad_pop_count, $miss_count := code→scores;
  time→now→post_to_wall;
  // ## Game loop
  while true do {
    $board→evolve;
    code→evolve;
    $board→update_on_wall;
  }
}
var \_board : Board {
}
var \_bubbles : Sprite_Set {
}

action add_bubble(bubble: Sprite) {
  // Adds a bubble
  data→\_bubbles→add($bubble);
  code→reset_bubble_pos($bubble);
}

action initialize(board: Board) {
  // Initializes the library
  data→\_board := $board;
  data→\_bubbles := data→\_board→create_sprite_set;
  data→\_bad_bubbles := data→\_board→create_sprite_set;
  data→\_touch := data→\_board→create_ellipse(20, 20);
  data→\_touch→hide;
  data→\_touch→set_friction(1);
  data→\_pop := invalid→sound;
  data→\_bad_pop := invalid→sound;
  code→set_physics(data→\_board→height / 3, data→\_board→height / 6);
  code→clear_scores;
}

action evolve() {
  // Updates the bubbles
  code→pop_bubbles;
  code→wrap_bubbles;
}

action set_physics(speed: Number, gravity: Number) {
  data→\_speed := $speed;
  data→\_gravity := $gravity;
}
var \_gravity : Number {
}

action bubbles() returns sprites: Sprite_Set {
  // Gets the set of bubbles
  $sprites := data→\_bubbles;
}
var \_touch : Sprite {
}

action pop_bubbles() {
  if data→\_board→touched then {
    $p := data→\_board→touch_current;
    data→\_touch→set_pos($p→x, $p→y);
    $to_pop := true;
    foreach bubble in data→\_touch→overlap_with(data→\_bubbles) where $to_pop do {
      $to_pop := false;
      data→\_pop_count := data→\_pop_count + 1;
      code→reset_bubble_pos($bubble);
      if not data→\_pop→is_invalid then {
        data→\_pop→play;
      }
    }
    if $to_pop then {
      foreach bad_bubble in data→\_touch→overlap_with(data→\_bad_bubbles) where $to_pop do {
        $to_pop := false;
        data→\_bad_pop_count := data→\_bad_pop_count + 1;
        code→reset_bubble_pos($bad_bubble);
        if not data→\_bad_pop→is_invalid then {
          data→\_bad_pop→play;
        }
        phone→vibrate(0.05);
      }
    }
  }
  meta private;
}
var \_pop : Sound {
}

action reset_bubble_pos(sprite: Sprite) {
  $sprite→set_speed(0, data→\_speed);
  $sprite→set_acceleration(0, data→\_gravity);
  $x := $sprite→width / 2 + math→random(data→\_board→width - $sprite→width);
  if data→\_gravity < 0 then {
    $sprite→set_pos($x, data→\_board→height + $sprite→height / 2 + 10 + math→random(50));
  }
  else {
    $sprite→set_pos($x, - $sprite→height / 2 - 10 - math→random(50));
  }
  meta private;
}

action add_bad_bubble(bubble: Sprite) {
  // Adds a bad bubble.
  data→\_bad_bubbles→add($bubble);
  code→reset_bubble_pos($bubble);
}
var \_bad_bubbles : Sprite_Set {
}

action bad__bubbles() returns sprites: Sprite_Set {
  // Gets the set of bad bubbles
  $sprites := data→\_bad_bubbles;
}
var \_bad_pop : Sound {
}

action set_pop_sound(sound: Sound) {
  // Sets the sound that plays when a bubble is popped.
  data→\_pop := $sound;
}

action set_bad_pop_sound(sound: Sound) {
  // Sets the sound that plays when a bad bubble is popped
  data→\_bad_pop := $sound;
}
var \_pop_count : Number {
}
var \_miss_count : Number {
}
var \_bad_pop_count : Number {
}

action scores() returns pop_count: Number, bad_pop_count: Number, miss_count: Number {
  // Gets the total number of bubble popped, bad bubble popped and missed bubbles.
  $pop_count := data→\_pop_count;
  $bad_pop_count := data→\_bad_pop_count;
  $miss_count := data→\_miss_count;
}

action clear_scores() {
  // Clears the scores
  data→\_pop_count := 0;
  data→\_bad_pop_count := 0;
  data→\_miss_count := 0;
}

action wrap_bubbles() {
  $b := data→\_gravity > 0;
  $h := data→\_board→height;
  foreach sprite in data→\_bubbles where true do {
    if($b and $sprite→y > $h + $sprite→height / 2) or ( not $b and $sprite→y < - $sprite→height / 2) then {
      code→reset_bubble_pos($sprite);
      data→\_miss_count := data→\_miss_count + 1;
    }
  }
  foreach sprite1 in data→\_bad_bubbles where true do {
    if($b and $sprite1→y > $h + $sprite1→height / 2) or ( not $b and $sprite1→y < - $sprite1→height / 2) then {
      code→reset_bubble_pos($sprite1);
    }
  }
  meta private;
}
var \_speed : Number {
}
var bad_pop_count : Number {
}
var pops : Number {
}
var miss_count : Number {
}
table thing {
  type = "Object";
}

