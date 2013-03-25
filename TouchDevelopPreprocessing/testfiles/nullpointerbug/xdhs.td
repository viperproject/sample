meta version "v2.2,nothing";
meta name "Rocket vs UFO";
meta icon "ArrowDownR";
meta color "#ff007FFF";
// Tap the screen to creates bubbles!
meta platform "current";

action main() {
  // The board is a 2D canvas with a physics engine. Gravity makes sprites fall and boundary makes them bounce.
  wall→set_background(colors→cyan);
  data→board := media→create_landscape_board(800, 480);
  data→board→set_background(colors→green);
  data→board→create_boundary(0);
  data→info := data→board→create_text(20, 14, 18, "Tap to Rocket Battle or I will slap your face");
  data→info→set_color(colors→black);
  data→Score_UFO := 0;
  data→UFO_Scoring := data→board→create_text(20, 14, 18, "UFO Score: " ∥ data→Score_UFO);
  data→UFO_Scoring→set_pos(500, 50);
  data→UFO_Scoring→set_color(colors→black);
  skip;
  data→Rocket_Scoring := data→board→create_text(20, 14, 18, "Rocket Score: " ∥ data→Score_Rocket);
  data→Rocket_Scoring→set_pos(500, 400);
  data→Rocket_Scoring→set_color(colors→black);
  data→Score_UFO := 0;
  data→Score_Rocket := 0;
  data→bRocket1Tapped := false;
  data→bRocket2Tapped := false;
  data→bRocket3Tapped := false;
  data→bRocketsTapped := false;
  data→bBoardTapped := false;
  data→Alien_Random_Nr := math→random(3);
  // Display the board. The game loop will start after leaving main.
  data→board→post_to_wall;
}
var board : Board {
}

event tap_board\u003A_board(x: Number, y: Number) {
  // When the board is tapped, create a circle sprite that falls down.
  skip;
  if not data→bBoardTapped then {
    time→log("Board tapped set to " ∥ data→bBoardTapped→to_string);
    wall→set_background(colors→blue);
    code→Set_Images;
    data→bBoardTapped := true;
    data→bRocketsTapped := false;
  }
  else {
    code→Reset;
  }
  meta private;
}
var Alien1 : Picture {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/ylmqqqbk";
}
var Alien2 : Picture {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/tjtcmwnp";
}
var Alien3 : Picture {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/mnrzimvi";
}
var Rocket1 : Picture {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/teumuvwf";
}
var Rocket2 : Picture {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/mnwsuwws";
}
var Rocket3 : Picture {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/alvymgdh";
}
var Alien1_Sprite : Sprite {
}
var Alien_2_sprite : Sprite {
}
var Alien_3_sprite : Sprite {
}
var Rocket_1_sprite : Sprite {
}
var Rocket_2_sprite : Sprite {
}
var Rocket_3_sprite : Sprite {
}
var info : Sprite {
}

event tap_sprite\u003A_Rocket_1_sprite(sprite: Sprite, x: Number, y: Number) {
  if not data→bRocketsTapped then {
    data→Generic_Rocket := data→Rocket_1_sprite;
    data→Generic_Rocket→set_pos(250, 250);
    code→Place_Alien_Ship;
    data→bRocketsTapped := true;
    code→Battle;
  }
  meta private;
}

event tap_sprite\u003A_Rocket_2_sprite(sprite: Sprite, x: Number, y: Number) {
  if data→bRocketsTapped→equals(false) then {
    data→Generic_Rocket := data→Rocket_2_sprite;
    data→Generic_Rocket→set_pos(250, 250);
    code→Place_Alien_Ship;
    data→bRocketsTapped := true;
    code→Battle;
  }
  meta private;
}

event tap_sprite\u003A_Rocket_3_sprite(sprite: Sprite, x: Number, y: Number) {
  if data→bRocketsTapped→equals(false) then {
    data→Generic_Rocket := data→Rocket_3_sprite;
    data→Generic_Rocket→set_pos(250, 250);
    code→Place_Alien_Ship;
    data→bRocketsTapped := true;
    code→Battle;
  }
  meta private;
}
var bRocket1Tapped : Boolean {
}
var bRocket2Tapped : Boolean {
}
var bRocket3Tapped : Boolean {
}
var bBoardTapped : Boolean {
}
var Alien_Random_Nr : Number {
}

action Place_Alien_Ship() {
  if data→Alien_Random_Nr = 0 then {
    data→Generic_Alien := data→Alien1_Sprite;
    data→Generic_Alien→set_pos(200, 200);
  }
  else {
    if data→Alien_Random_Nr = 1 then {
      data→Generic_Alien := data→Alien_2_sprite;
      data→Generic_Alien→set_pos(200, 200);
    }
    else {
      if data→Alien_Random_Nr = 2 then {
        data→Generic_Alien := data→Alien_3_sprite;
        data→Generic_Alien→set_pos(200, 200);
      }
    }
  }
  data→info→set_text("Game In Play");
  meta private;
}
var bRocketsTapped : Boolean {
}

event gameloop() {
  data→board→evolve;
  data→board→update_on_wall;
  meta private;
}

action Battle() {
  data→Dice_Alien_Throw := math→random(7);
  data→Dice_Rocket_Throw := math→random(7);
  if data→Dice_Alien_Throw > data→Dice_Rocket_Throw then {
    data→info→set_text("UFO wins!\n\n\nTap Screen twice to Battle again\nNot the Rockets or UFOs!");
    data→Score_UFO := data→Score_UFO + 1;
    data→UFO_Scoring→set_text("UFO Score: " ∥ data→Score_UFO→to_string);
  }
  else {
    if data→Dice_Rocket_Throw = data→Dice_Alien_Throw then {
      data→info→set_text("Both Destroyed\n\n\nTap Screen Twice to Battle again\nNot the Rockets or UFOs!");
      data→Score_Rocket := data→Score_Rocket - 1;
      data→Rocket_Scoring→set_text("Rocket Score: " ∥ data→Score_Rocket);
      data→Score_UFO := data→Score_UFO - 1;
      data→UFO_Scoring→set_text("UFO Score: " ∥ data→Score_UFO→to_string);
    }
    else {
      data→info→set_text("Rocket Wins!\n\n\nTap Screen Twice to Battle again\nNot the Rockets or UFOs!");
      data→Score_Rocket := data→Score_Rocket + 1;
      data→Rocket_Scoring→set_text("Rocket Score: " ∥ data→Score_Rocket);
    }
  }
  skip;
  data→board→update_on_wall;
  meta private;
}
var Alien_Girl : Picture {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/mlqrlrhl";
}

action Set_Images() {
  data→Alien1_Sprite := data→board→create_picture(art→Alien1);
  data→Alien1_Sprite→set_pos(100, 50);
  data→Alien1_Sprite→set_height(50);
  data→Alien_2_sprite := data→board→create_picture(art→Alien2);
  data→Alien_2_sprite→set_pos(200, 50);
  data→Alien_2_sprite→set_height(50);
  data→Alien_3_sprite := data→board→create_picture(art→Alien3);
  data→Alien_3_sprite→set_pos(300, 50);
  data→Alien_3_sprite→set_height(50);
  data→Rocket_1_sprite := data→board→create_picture(art→Rocket1);
  data→Rocket_1_sprite→set_pos(100, 400);
  data→Rocket_1_sprite→set_height(75);
  data→Rocket_2_sprite := data→board→create_picture(art→Rocket2);
  data→Rocket_2_sprite→set_pos(200, 400);
  data→Rocket_2_sprite→set_height(75);
  data→Rocket_3_sprite := data→board→create_picture(art→Rocket3);
  data→Rocket_3_sprite→set_pos(300, 400);
  data→Rocket_3_sprite→set_height(75);
  data→info→set_text("Now Tap One of the Rockets on \nbottom of the screen");
  meta private;
}

action Reset() {
  time→log("In reset");
  if not data→Generic_Rocket→is_invalid then {
    data→Generic_Rocket→delete;
    data→Generic_Rocket := invalid→sprite;
  }
  if not data→Generic_Alien→is_invalid then {
    data→Generic_Alien→delete;
    data→Generic_Alien := invalid→sprite;
  }
  data→bBoardTapped := false;
  data→Alien_Random_Nr := math→random(3);
  data;
  data→board→update_on_wall;
  meta private;
}
var Generic_Alien : Sprite {
}
var Generic_Rocket : Sprite {
}
var Dice_Alien_Throw : Number {
}
var Dice_Rocket_Throw : Number {
}
var Score_UFO : Number {
}
var Score_Rocket : Number {
}
var UFO_Scoring : Sprite {
}
var Rocket_Scoring : Sprite {
}

