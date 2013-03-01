meta version "v2.2,nothing";
meta name "Heads Up Display";
meta icon "dashboard";
meta color "#ff008000";
// Shows your current speed and heading. Updates once per second.
meta platform "current";

action updateSpeed() {
  $mySpeed := math→round(senses→current_location_accurate→speed * 3.6 * 0.621371192);
  $loc := senses→current_location_accurate;
  $loc→speed;
  data→txtSpeed→set_color(colors→dark_gray);
  if $mySpeed ≤ 70 then {
    data→txtSpeed→set_color(colors→green);
  }
  else {
    if $mySpeed > 70 and $mySpeed ≤ 75 then {
      data→txtSpeed→set_color(colors→yellow);
    }
    else {
      if $mySpeed > 75 then {
        data→txtSpeed→set_color(colors→red);
      }
      else {
        if math→is_nan($mySpeed) then {
          $mySpeed := - 1;
        }
        else {
          if $mySpeed→is_invalid then {
            data→txtSpeed→set_color(colors→magenta);
          }
        }
      }
    }
  }
  data→mphLabel→set_color(data→txtSpeed→color);
  data→txtSpeed→set_text($mySpeed ∥ "");
  data→board→update_on_wall;
  meta private;
}

action main() {
  //
  ... ;
  // Sets up the board and the text boxes used to display the compass direction and current speed.
  code→setUpDisplay;
  //
  // The text boxes are updated by their respective actions which are continuously invoked by the gameloop event.
}
var txtSpeed : Sprite {
}
var txtDirection : Sprite {
}

action updateDirection() {
  $prev := data→txtDirection→text;
  $myDir := senses→heading;
  data→numDirection→set_text($myDir ∥ "");
  if $myDir ≤ 22.5 or $myDir ≥ 337.5 then {
    data→txtDirection→set_text("N");
  }
  else {
    if $myDir > 22.5 and $myDir < 67.5 then {
      data→txtDirection→set_text("Ne");
    }
    else {
      if $myDir ≥ 67.5 and $myDir ≤ 112.5 then {
        data→txtDirection→set_text("E");
      }
      else {
        if $myDir > 112.5 and $myDir < 157.5 then {
          data→txtDirection→set_text("Se");
        }
        else {
          if $myDir ≥ 157.5 and $myDir ≤ 202.5 then {
            data→txtDirection→set_text("S");
          }
          else {
            if $myDir > 202.5 and $myDir < 247.5 then {
              data→txtDirection→set_text("Sw");
            }
            else {
              if $myDir ≥ 247.5 and $myDir ≤ 292.5 then {
                $s := "";
                data→txtDirection→set_text("W");
              }
              else {
                if $myDir > 292.5 and $myDir < 337.5 then {
                  data→txtDirection→set_text("Nw");
                }
              }
            }
          }
        }
      }
    }
  }
  if data→txtDirection→text→contains("N") then {
    data→txtDirection→set_color(colors→green);
  }
  else {
    if data→txtDirection→text→contains("S") then {
      data→txtDirection→set_color(colors→red);
    }
    else {
      if data→txtDirection→text→contains("E") then {
        data→txtDirection→set_color(colors→orange);
      }
      else {
        if data→txtDirection→text→contains("W") then {
          data→txtDirection→set_color(colors→yellow);
        }
      }
    }
  }
  data→board→update_on_wall;
  meta private;
}
var numDirection : Sprite {
}

action setUpDisplay() {
  //
  // Create the sprite set.
  data→board := media→create_full_board;
  data→board→post_to_wall;
  data→txtBoxes := data→board→create_sprite_set;
  //
  // ----------------------------------------------
  // Create the text box to show the current direction.
  data→txtDirection := data→board→create_text(data→board→width, data→board→height * .25, 150, "");
  data→txtDirection→set_pos(data→board→width * .5, data→board→height * .15);
  data→txtBoxes→add(data→txtDirection);
  //
  // Create and populate the title bar over the direction.
  $dirTitleBox := data→board→create_rectangle(data→board→width, 30);
  $dirTitleBox→set_pos(data→board→width * .5, $dirTitleBox→height);
  $dirTitleBox→set_color(colors→dark_gray→darken(0.5));
  $dirTitleTxt := data→board→create_text($dirTitleBox→width, $dirTitleBox→height, 20, "Direction");
  $dirTitleTxt→set_pos($dirTitleBox→width * .5, ($dirTitleBox→height));
  $dirTitleTxt→set_color(colors→yellow);
  data→numDirection := data→board→create_text(25, 25, 15, "--");
  data→numDirection→set_pos(data→board→width * .5, data→board→height * .26);
  data→numDirection→set_color(colors→dark_gray);
  data→txtBoxes→add(data→numDirection);
  //
  // ---------------------------------------------------
  // Create the text box to show the current speed.
  data→txtSpeed := data→board→create_text(data→board→width, data→board→height * .5, 400, "00");
  data→txtSpeed→set_pos(data→board→width * .5, data→board→height * .5);
  data→txtBoxes→add(data→txtSpeed);
  //
  // Create and populate the title bar over the speed.
  $speedTitleBox := data→board→create_rectangle(data→board→width, 30);
  $speedTitleBox→set_pos(data→board→width * .5, data→board→height * .30);
  $speedTitleBox→set_color(colors→dark_gray→darken(0.5));
  $speedTitleTxt := data→board→create_text($dirTitleBox→width, $dirTitleBox→height, 20, "Speed");
  $speedTitleTxt→set_pos($dirTitleBox→width * .5, (data→board→height * .3));
  $speedTitleTxt→set_color(colors→yellow);
  //
  data→mphLabel := data→board→create_text(100, 100, 40, "mph");
  data→mphLabel→set_pos(data→board→width * .5, data→board→height * .9);
  data→mphLabel→set_color(colors→yellow);
  //
  // Update the wall with the board and sprites.
  data→board→update_on_wall;
  meta private;
}
var board : Board {
}
var direction : Sprite {
}
var txtBoxes : Sprite_Set {
}
var speed : Sprite {
}

event gameloop() {
  // This event continuously calls the actions designed to update the Speed and Direction text boxes, with a 1 second delay between updates.
  //
  code→updateDirection;
  code→updateSpeed;
  time→sleep(1);
  meta private;
}
var mphLabel : Sprite {
}

