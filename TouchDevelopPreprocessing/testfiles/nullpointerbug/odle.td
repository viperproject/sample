meta version "v2.2,nothing";
meta name "Line Runner";
meta icon "runningman";
meta color "#ff007fff";
// Work In Progress...
meta platform "current";

action main() {
  data→loops := 0;
  data→loopshole := 0;
  data→board := media→create_landscape_board(800, 480);
  data→board→create_text(100, 20, 70, "Beta - Work In Progress")→set_pos(50, 50);
  data→board→set_background(colors→white);
  data→ground := data→board→create_rectangle(800, 180);
  data→ground→set_pos(400, 390);
  data→ground→set_color(colors→black);
  data→person := data→board→create_rectangle(50, 80);
  data→person→set_color(colors→green);
  data→person→set_pos(200, 260);
  data→personroll := data→board→create_ellipse(40, 40);
  data→personroll→set_color(colors→green);
  data→personroll→set_pos(200, 280);
  data→personroll→hide;
  data→onground := true;
  data→rolling := false;
  data→screen := data→board→create_rectangle(800, 480);
  data→screen→set_color(colors→transparent);
  data→board→post_to_wall;
  data→objects := collections→create_number_collection;
  data→objects→clear;
  data→object1 := data→board→create_rectangle(20, 20);
  data→object1→set_color(colors→black);
  data→object1→hide;
  data→object2 := data→board→create_rectangle(20, 20);
  data→object2→set_color(colors→black);
  data→object2→hide;
  data→object3 := data→board→create_rectangle(20, 20);
  data→object3→set_color(colors→black);
  data→object3→hide;
}
var board : Board {
}
var person : Sprite {
}

event gameloop() {
  data→loops := data→loops + 1;
  code→personcheck;
  code→personnormalise;
  code→objectcheck;
  code→objectspawn;
  data→board→evolve;
  data→board→update_on_wall;
  meta private;
}
var loops : Number {
}
var swipe : Number {
}
var ground : Sprite {
}
var screen : Sprite {
}
var onground : Boolean {
}
var loopsup : Number {
}

event tap_sprite\u003A_screen(sprite: Sprite, x: Number, y: Number) {
  if $x > 400 then {
    if data→onground then {
      data→loopsup := data→loops;
      data→onground := false;
      data→person→set_speed(0, - 600);
      data→person→set_acceleration(0, 1500);
    }
  }
  else {
    if not data→rolling then {
      data→loopsroll := data→loops;
      data→rolling := true;
      data→person→hide;
      data→personroll→show;
    }
  }
  meta private;
}
var object1 : Sprite {
}
var loopshole : Number {
}
var personroll : Sprite {
}
var rolling : Boolean {
}
var loopsroll : Number {
}

action objectspawn() {
  if data→loops > data→loopshole + 14 and math→random(4) = 0 then {
    data→loopshole := data→loops;
    if not data→objects→contains(1) then {
      data→objects→add(1);
      if math→random(2) = 0 then {
        data→object1→set_pos(810, 290);
      }
      else {
        data→object1→set_pos(810, 230);
      }
      data→object1→set_speed( - 500, 0);
      data→object1→show;
    }
    else {
      if not data→objects→contains(2) then {
        data→objects→add(2);
        if math→random(2) = 0 then {
          data→object2→set_pos(810, 290);
        }
        else {
          data→object2→set_pos(810, 230);
        }
        data→object2→set_speed( - 500, 0);
        data→object2→show;
      }
      else {
        if not data→objects→contains(3) then {
          data→objects→add(3);
          if math→random(2) = 0 then {
            data→object3→set_pos(810, 290);
          }
          else {
            data→object3→set_pos(810, 230);
          }
          data→object3→set_speed( - 500, 0);
          data→object3→show;
        }
      }
    }
  }
  meta private;
}

action personnormalise() {
  if data→onground→equals(false) and data→loops > data→loopsup + 12 then {
    data→onground := true;
    data→person→set_pos(200, 260);
    data→person→set_speed(0, 0);
    data→person→set_acceleration(0, 0);
  }
  if data→rolling and data→loops > data→loopsroll + 12 then {
    data→rolling := false;
    data→personroll→hide;
    data→person→show;
  }
  meta private;
}

action personcheck() {
  if data→rolling then {
    if data→object1→overlaps_with(data→personroll) or data→object2→overlaps_with(data→personroll) or data→object3→overlaps_with(data→personroll) then {
      time→sleep(3);
    }
  }
  else {
    if data→object1→overlaps_with(data→person) or data→object2→overlaps_with(data→person) or data→object3→overlaps_with(data→person) then {
      time→sleep(3);
    }
  }
  meta private;
}
var object2 : Sprite {
}
var object3 : Sprite {
}
var objects : Number_Collection {
}

action objectcheck() {
  if data→object1→x < - 10 then {
    data→objects→remove(1);
    data→object1→set_speed(0, 0);
    if math→random(2) = 0 then {
      data→object1→set_pos(810, 290);
    }
    else {
      data→object1→set_pos(810, 230);
    }
    data→object1→hide;
  }
  if data→object2→x < - 10 then {
    data→objects→remove(2);
    data→object2→set_speed(0, 0);
    if math→random(2) = 0 then {
      data→object2→set_pos(810, 290);
    }
    else {
      data→object2→set_pos(810, 230);
    }
    data→object2→hide;
  }
  if data→object3→x < - 10 then {
    data→objects→remove(3);
    data→object3→set_speed(0, 0);
    if math→random(2) = 0 then {
      data→object3→set_pos(810, 290);
    }
    else {
      data→object3→set_pos(810, 230);
    }
    data→object3→hide;
  }
  meta private;
}

