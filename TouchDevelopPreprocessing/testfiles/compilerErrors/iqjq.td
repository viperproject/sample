meta version "v2.2,nothing";
meta name "Smoking Monitor";
meta icon "delete";
// Monitors your smoking to help you quit. No nagging, no medical info, just the things that help me.
//
// Includes:
// Tile counter.
// Timer to try to outlast your craving.
// Graphical amount smoked for 28 days, with your target displayed.
// Optional automatic target reduction.
// Time since last smoked.
// Money saved.
//
// On a new day the tile counter will update when you open the app.
meta platform "current";

action main() {
  // Sets initial parameters, checks if it is a new day and calls all the board setup procedures.
  // Sets the main tile to theme colour
  data→main_tile→set_background(colors→accent);
  // The horizontal position on which the graph is based
  data→graph_hpostion := data→board→width / 2 + 203;
  // Sets up a new day if required
  if records→history_store_index→at(1)→smkdte→get→year_day ≠ time→today→year_day then {
    code→new_day_setup;
  }
  // Creates the app board
  code→board_setup;
  // Finds when you last smoked
  code→last_smoked_setup;
  // Sets up the graph display
  code→graph_initial_setup;
  // Sets the target line on the graph
  code→target_setup;
  // Sets the counter on the main tile
  data→main_tile→set_counter(records→history_store_index→at(1)→smoked→get);
  // Puts every thing on the wall
  data→board→post_to_wall;
  // Adds a page button to reset data
  wall→add_button("edit", "Settings");
}
var main_tile : Tile {
  // The main tile
  readonly = true;
}
var timer_display : Sprite {
  // Displays a 3 minute timer
}
var board : Board {
  // The display space
}
var timer_minutes : Number {
  // The minutes for the timer
}
var timer_seconds : Number {
  // The seconds for the timer
}

event tap_sprite\u003A_text_smoke_now(sprite: Sprite, x: Number, y: Number) {
  // Increments and stores the daily count, the tile counter and the main screen text.
  // Vibrates for press feedback
  phone→vibrate(0.1);
  // Adds one to the total smoked today
  records→history_store_index→at(1)→smoked→add(1);
  // Adds one to the complete total smoked
  records→settings_index→at(1)→total_smoked→add(1);
  code→total_saved_setup;
  // Set the last time smoked
  records→history_store_index→at(1)→last_smoked→set(time→now);
  // Adds one to the main tile counter
  data→main_tile→set_counter(records→history_store_index→at(1)→smoked→get);
  // Displays the amended text on the main screen
  data→text_daily_count→set_text("You\'ve smoked " ∥ records→history_store_index→at(1)→smoked→get ∥ " today");
  // Calls for the graph to be updated
  code→graph_update;
  // Calls for the target line to be updated
  code→target_setup;
  // Calls for the last smoked time to be updated
  data→text_last_smoked→set_text("Last smoked 0 d 0 h 0 mins ago");
  // Posts the graph & last smoked time to the board
  data→board→update_on_wall;
  meta private;
}
var text_smoke_now : Sprite {
  // The smoke now button
}
table history_store {
  // Contains daily totals
  type = "Index";
  keys {
    key: Number
  }
  fields {
    smoked: Number_field
    smkdte: DateTime_field
    last_smoked: DateTime_field
  }
}

action new_day_setup() {
  // Moves all the previous days values down one in the store and sets the new day to zero. Adjusts savings and checks automatic reduction
  // Limits the number of stored entries to 28
  if records→history_store_index→count ≥ 28 then {
    records→history_store_index→at(28)→clear_fields;
  }
  else {
    // Defines initial pointer variables
    $j := records→history_store_index→count;
    // Moves all the stored data down one in the store
    while $j > 0 do {
      // Moves the number smoked down one
      $x2 := records→history_store_index→at($j)→smoked→get;
      records→history_store_index→at($j + 1)→smoked→set($x2);
      // Moves the date down one
      $x1 := records→history_store_index→at($j)→smkdte→get;
      records→history_store_index→at($j + 1)→smkdte→set($x1);
      // Adjusts the pointer variables
      $j := $j - 1;
    }
    // Sets the new day values to 0 and today
    records→history_store_index→at(1)→smoked→set(0);
    records→history_store_index→at(1)→smkdte→set(time→now);
  }
  // Adds one to the total days for savings calculation
  records→settings_index→at(1)→total_days→add(1);
  // Adds one to the automatic target reduction days and checks it
  if records→settings_index→at(1)→reduction→get > 1 then {
    records→settings_index→at(1)→reduction_days→add(1);
    code→settings_target_automatic(records→settings_index→at(1)→reduction→get, records→settings_index→at(1)→reduction_days→get);
  }
  meta private;
}

event tap_wall_Page_Button(item: Page_Button) {
  if $item→text→equals("Settings") then {
    code→settings_board;
  }
  meta private;
}

action board_setup() {
  // Sets up the screen display.
  // Sets the initial values for the timer
  data→timer_minutes := 3;
  data→timer_seconds := 0;
  // Defines the board
  data→board := media→create_board(640);
  data→board→set_background(colors→chrome);
  // Defines the title
  $title := data→board→create_text(data→board→width - 10, 50, 50, "Smoking Monitor");
  $title→set_pos(data→board→width / 2, 15);
  // Defines the text for smoked today
  data→text_daily_count := data→board→create_text(data→board→width - 10, 36, 36, "You\'ve smoked " ∥ records→history_store_index→at(1)→smoked→get ∥ " today");
  data→text_daily_count→set_color(colors→orange);
  data→text_daily_count→set_pos(data→board→width / 2, 75);
  // Defines the text describing the timer
  $timer_description := data→board→create_text(300, 40, 20, "Cravings normally last for 3 mins.\nTry to outlast them, tap the timer.");
  $timer_description→set_pos(285, 140);
  // The initial timer display
  $timer_back := data→board→create_rectangle(110, 55);
  $timer_back→set_color(colors→orange→make_transparent(0.7));
  $timer_back→set_pos(70, 150);
  data→timer_display := data→board→create_text(100, 50, 50, data→timer_minutes ∥ ":0" ∥ data→timer_seconds);
  data→timer_display→set_color(colors→chrome);
  data→timer_display→set_pos(70, 138);
  // Defines the total saved text
  data→text_total_saved := data→board→create_text(data→board→width - 10, 25, 25, " ");
  code→total_saved_setup;
  data→text_total_saved→set_pos(data→board→width / 2, 210);
  // Defines the target line
  data→target_line := data→board→create_rectangle((data→board→width / 2) + (data→board→width / 2 - 30), 2);
  // Defines the smoke now display
  $smoke_now_back := data→board→create_rectangle(340, 60);
  $smoke_now_back→set_color(colors→orange→make_transparent(0.7));
  $smoke_now_back→set_pos(data→board→width / 2, 592);
  data→text_smoke_now := data→board→create_text(400, 20, 60, "Smoke Now");
  data→text_smoke_now→set_color(colors→chrome);
  data→text_smoke_now→set_pos((data→board→width / 2), 560);
  meta private;
}
var graph_line : Sprite {
  // Graph axis line
}

action graph_initial_setup() {
  // Sets up all the graph elements on the screen.
  // Calls the code to set the graph scaling parameter
  code→graph_scaling;
  // Defines the sprite sets for the graph labels & elements
  $hposition := data→graph_hpostion;
  data→graph_elements := data→board→create_sprite_set;
  data→graph_labels := data→board→create_sprite_set;
  // Iterates through the graph elements, setting colour, position and size
  for 0 ≤ i < 28 do {
    // Stores the 30 graph elements & sets the dimensions
    data→graph_elements→add(data→board→create_rectangle(15, records→history_store_index→at($i + 1)→smoked→get * data→graph_scaler));
    // Sets the position of the graph elements
    data→graph_elements→at($i)→set_pos($hposition, 539 - (records→history_store_index→at($i + 1)→smoked→get * data→graph_scaler) / 2);
    // Sets the colour of the graph elements
    data→graph_elements→at($i)→set_color(colors→blue);
    // Stores the 30 graph labels
    data→graph_labels→add(data→board→create_text(15, 15, 14, ""));
    // Sets the position of the graph labels
    data→graph_labels→at($i)→set_pos($hposition, 528);
    // Sets the colour of the graph labels
    data→graph_labels→at($i)→set_color(colors→white);
    // Sets the text of the graph labels
    if records→history_store_index→at($i + 1)→smoked→get = 0 then {
      data→graph_labels→at($i)→set_text(" ");
    }
    else {
      data→graph_labels→at($i)→set_text(records→history_store_index→at($i + 1)→smoked→get→to_string);
    }
    // Reduces the position for the next element
    $hposition := $hposition - 15;
  }
  // Defines and places the graph line
  data→graph_line := data→board→create_rectangle((data→board→width / 2) + (data→board→width / 2 - 20), 2);
  data→graph_line→set_pos(data→board→width / 2, 540);
  meta private;
}

action graph_update() {
  // Updates the graph.
  // Stores a local version of the graph scaler for comparison
  $hposition := data→graph_hpostion;
  // Sets a local scaling parameter parameter for comparison
  $local_scaler := data→graph_scaler;
  // Calls the code to set the graph scaling parameter
  code→graph_scaling;
  // Updates today if the graph scaler hasn't changed
  if $local_scaler = data→graph_scaler then {
    // Sets the height of the graph element for today
    data→graph_elements→at(0)→set_height(records→history_store_index→at(1)→smoked→get * data→graph_scaler);
    // Sets the position for today
    data→graph_elements→at(0)→set_pos(data→graph_hpostion, 539 - ((records→history_store_index→at(1)→smoked→get * data→graph_scaler) / 2));
    // Sets the graph label for today
    data→graph_labels→at(0)→set_text(records→history_store_index→at(1)→smoked→get→to_string);
  }
  else {
    for 0 ≤ i < 28 do {
      // Sets the height of the graph elements
      data→graph_elements→at($i)→set_height(records→history_store_index→at($i + 1)→smoked→get * data→graph_scaler);
      // Sets the position of the graph elements
      data→graph_elements→at($i)→set_pos($hposition, 539 - (records→history_store_index→at($i + 1)→smoked→get * data→graph_scaler) / 2);
      // Sets the text of the graph labels
      if records→history_store_index→at($i + 1)→smoked→get = 0 then {
        data→graph_labels→at($i)→set_text(" ");
      }
      else {
        data→graph_labels→at($i)→set_text(records→history_store_index→at($i + 1)→smoked→get→to_string);
      }
      // Reduces the position for the next element
      $hposition := $hposition - 15;
    }
  }
  meta private;
}
var graph_scaler : Number {
  // Number to scale the graph
}

action graph_scaling() {
  // Sets a graph scaling parameter based on the max smoked over the last 30 days.
  // Searches the history and sets j to the max smoked
  $j := 0;
  for 0 ≤ i1 < 28 do {
    if records→history_store_index→at($i1)→smoked→get > $j then {
      $j := records→history_store_index→at($i1)→smoked→get;
    }
  }
  if records→settings_index→at(1)→target→get > $j then {
    $j := records→settings_index→at(1)→target→get;
  }
  // Sets the scaling parameter based on max smoked
  if $j > 60 then {
    data→graph_scaler := 2;
  }
  else {
    if $j > 50 then {
      data→graph_scaler := 4;
    }
    else {
      if $j ≥ 42 then {
        data→graph_scaler := 5;
      }
      else {
        if $j ≥ 32 then {
          data→graph_scaler := 6;
        }
        else {
          if $j ≥ 22 then {
            data→graph_scaler := 8;
          }
          else {
            if $j > 10 then {
              data→graph_scaler := 12;
            }
            else {
              data→graph_scaler := 25;
            }
          }
        }
      }
    }
  }
  meta private;
}
var graph_elements : Sprite_Set {
  // The graph rectangle elements
}
var graph_labels : Sprite_Set {
  // The graph labels
}

event page_navigated_from() {
  // Creates the app board
  code→board_setup;
  // Sets up the graph display
  code→graph_initial_setup;
  // Sets up the last smoked time
  code→last_smoked_setup;
  // Sets up the target line
  code→target_setup;
  // Calls total saved
  code→total_saved_setup;
  // Sets the counter on the main tile
  data→main_tile→set_counter(records→history_store_index→at(1)→smoked→get);
  // Puts every thing on the wall
  data→board→post_to_wall;
  meta private;
}
var graph_hpostion : Number {
  // The horizontal position at which the graph layout is based
}
var text_last_smoked : Sprite {
  // The date/time last smoked
}
var dtg_last_smoked : DateTime {
  // The date/time last smoked
}

action last_smoked_setup() {
  // Sets the time and label for last smoked
  // Sets the last smoked variable
  data→dtg_last_smoked := time→create(0001, 01, 01, 12, 00, 00);
  // A comparison with the history, storing last smoked date/time
  for 0 ≤ i < 28 do {
    if records→history_store_index→at($i + 1)→last_smoked→get→greater(data→dtg_last_smoked) then {
      data→dtg_last_smoked := records→history_store_index→at($i + 1)→last_smoked→get;
    }
  }
  // If year is 0001, sets the last smoked label to nothing
  if data→dtg_last_smoked→year = 0001 then {
    data→text_last_smoked := data→board→create_text(400, 25, 25, " ");
  }
  else {
    // Sets up the last smoked label
    // Finds the time since last smoked in seconds
    $ttime_secs := math→floor(time→now→subtract(data→dtg_last_smoked));
    // Finds the time since last smoked in minutes
    $time_min := math→floor($ttime_secs / 60);
    // Initial set up of hours and days
    $time_hour := 0;
    $time_days := 0;
    // So long as the minutes are greater than 59 ...
    while $time_min > 59 do {
      // Adds one to the hour & deducts 60 from minutes
      $time_hour := $time_hour + 1;
      $time_min := $time_min - 60;
    }
    // So long as the hours are greater than 23 ...
    while $time_hour > 23 do {
      // Adds one to the day & deducts 24 from hours
      $time_days := $time_days + 1;
      $time_hour := $time_hour - 24;
    }
    // Defines the text for the last smoked label
    data→text_last_smoked := data→board→create_text(400, 25, 25, "Last smoked " ∥ $time_days ∥ " d " ∥ $time_hour ∥ " h " ∥ $time_min ∥ " mins ago");
  }
  // Sets position of last smoked text
  data→text_last_smoked→set_pos(data→board→width / 2, 255);
  meta private;
}

action test() {
  for 0 ≤ i < 28 do {
    records→history_store_index→at($i + 1)→smoked→set(math→random(15 + 5));
    records→history_store_index→at($i + 1)→smkdte→set(time→now→add_days( - 1));
    records→history_store_index→at($i + 1)→last_smoked→set(time→now→add_days( - 1));
  }
  meta private;
}
table settings {
  // Stores the users settings
  type = "Index";
  keys {
    settings_key: Number
  }
  fields {
    current: Number_field
    target: Number_field
    cost_packet: Number_field
    total_smoked: Number_field
    total_days: Number_field
    reduction: Number_field
    reduction_days: Number_field
  }
}
var button_up : Picture {
  is\_resource = true;
  url = "https://sn2files.storage.live.com/y1pjOsxKpv3LXJ6e5jQ933xuXPpN0s_HBmLukUZmuDbM79ezNZD_ryegA/btnupsmall.png.jpg?psid=1&ck=0&ex=720";
}
var button_down : Picture {
  is\_resource = true;
  url = "https://sn2files.storage.live.com/y1pjOsxKpv3LXKz40W8xekEuZvLNElhycudkh4vnBcXfar8Zpr_nUZq0F20G5Lx8tPAwEzxXFjd-3xDO8S5En7iFA/btndownsmall.png.jpg?psid=1";
}
var toggle_off : Picture {
  is\_resource = true;
  url = "https://sn2files.storage.live.com/y1p7vr1XokC_R94VYKLbpA2r331ZH03YWAs3kdJAKByfB6nGNGLszuiGw/toggleoffsmall.png.jpg?psid=1&ck=0&ex=720";
}
var toggle_on : Picture {
  is\_resource = true;
  url = "https://sn2files.storage.live.com/y1p7vr1XokC_R_q53hhTWvvzcL1csO8RrXPiGWDml_viENOco-KR6kbmw/toggleonsmall.png.jpg?psid=1&ck=0&ex=720";
}
var board_settings : Board {
  // The board for user settings
}

action settings_board() {
  // Sets up the settings page
  // Defines the wall
  wall→push_new_page;
  wall→set_title("Settings");
  // Defines the board
  data→board_settings := media→create_board(640);
  data→board_settings→set_background(colors→chrome);
  // Defines the label for currently smoking
  $settings_lbl1 := data→board_settings→create_text(data→board_settings→width - 10, 20, 20, "How many cigarettes do you smoke per day?");
  $settings_lbl1→set_pos(data→board_settings→width / 2, 25);
  // Defines the value display of currently smoking
  data→settings_current := data→board_settings→create_text(100, 40, 40, records→settings_index→at(1)→current→get→to_string);
  data→settings_current→set_pos(data→board_settings→width / 2, 70);
  // Defines up/down buttons for currently smoked
  data→settings_up1 := data→board_settings→create_picture(art→button_up);
  data→settings_down1 := data→board_settings→create_picture(art→button_down);
  data→settings_up1→set_pos(data→board_settings→width / 3 - 20, 80);
  data→settings_down1→set_pos(data→board_settings→width / 3 * 2 + 20, 80);
  // Defines the label for target smoking
  $settings_lbl2 := data→board_settings→create_text(data→board_settings→width - 10, 20, 20, "What is your target amount per day?");
  $settings_lbl2→set_pos(data→board_settings→width / 2, 140);
  // Defines the value display of target smoking
  data→settings_target := data→board_settings→create_text(100, 40, 40, records→settings_index→at(1)→target→get→to_string);
  data→settings_target→set_pos(data→board_settings→width / 2, 190);
  // Defines up/down buttons for target smoking
  data→settings_up2 := data→board_settings→create_picture(art→button_up);
  data→settings_down2 := data→board_settings→create_picture(art→button_down);
  data→settings_up2→set_pos(data→board_settings→width / 3 - 20, 200);
  data→settings_down2→set_pos(data→board_settings→width / 3 * 2 + 20, 200);
  // Defines the label for packet cost
  $settings_lbl3 := data→board_settings→create_text(data→board_settings→width - 10, 20, 20, "How much is a packet of cigarettes?");
  $settings_lbl3→set_pos(data→board_settings→width / 2, 260);
  // Defines the value display of target smoking
  if math→mod(records→settings_index→at(1)→cost_packet→get, 1) = 0 then {
    data→settings_cost := data→board_settings→create_text(100, 40, 40, records→settings_index→at(1)→cost_packet→get→to_string ∥ ".00");
  }
  else {
    data→settings_cost := data→board_settings→create_text(100, 40, 40, records→settings_index→at(1)→cost_packet→get→to_string ∥ "0");
  }
  data→settings_cost→set_pos(data→board_settings→width / 2, 310);
  // Defines up/down buttons for target smoking
  data→settings_up3 := data→board_settings→create_picture(art→button_up);
  data→settings_down3 := data→board_settings→create_picture(art→button_down);
  data→settings_up3→set_pos(data→board_settings→width / 3 - 20, 320);
  data→settings_down3→set_pos(data→board_settings→width / 3 * 2 + 20, 320);
  // Defines the label for target reduction
  $settings_lbl5 := data→board_settings→create_text(data→board_settings→width - 10, 20, 20, "Automatically reduce your target?");
  $settings_lbl5→set_pos(data→board_settings→width / 2, 380);
  // Defines the radio buttons
  code→settings_radios_initial;
  // Defines the radio button labels
  $smoke_no := data→board_settings→create_text(50, 20, 20, "No");
  $smoke_no→set_pos(data→board_settings→width / 5, 455);
  $weekly := data→board_settings→create_text(100, 20, 20, "Weekly");
  $weekly→set_pos(data→board_settings→width / 5 * 2, 455);
  $biweekly := data→board_settings→create_text(100, 20, 20, "Biweekly");
  $biweekly→set_pos(data→board_settings→width / 5 * 3, 455);
  $monthly := data→board_settings→create_text(100, 20, 20, "Monthly");
  $monthly→set_pos(data→board_settings→width / 5 * 4, 455);
  // Defines the label for clearing all data
  $settings_lbl4 := data→board_settings→create_text(data→board_settings→width - 10, 20, 20, "CAUTION: Swipe toggle to clear all stored data.");
  $settings_lbl4→set_color(colors→orange);
  $settings_lbl4→set_pos(data→board_settings→width / 2, 500);
  // Defines the toggle for clearing data
  data→settings_toggle := data→board_settings→create_picture(art→toggle_off);
  data→settings_toggle→set_pos(data→board_settings→width / 2, 550);
  // Posts the board to the wall
  data→board_settings→post_to_wall;
  meta private;
}
var text_daily_count : Sprite {
  // The daily amount smoked
}

event tap_sprite\u003A_timer_display(sprite: Sprite, x: Number, y: Number) {
  // Starts and runs the timer.
  // Vibrates for press feedback
  phone→vibrate(0.1);
  // Runs and displays the timer
  while data→timer_minutes ≠ 0 or data→timer_seconds ≠ 0 do {
    // Pauses for a second
    time→sleep(1);
    // Displays the timer when seconds are two digits
    if data→timer_seconds ≥ 10 then {
      data→timer_display→set_text(data→timer_minutes ∥ ":" ∥ data→timer_seconds);
      data→timer_seconds := data→timer_seconds - 1;
    }
    else {
      // Displays the timer when seconds are one digits
      if data→timer_seconds < 10 and data→timer_seconds ≥ 0 then {
        data→timer_display→set_text(data→timer_minutes ∥ ":0" ∥ data→timer_seconds);
        data→timer_seconds := data→timer_seconds - 1;
      }
      else {
        // Displays the timer and reduces minutes by one
        data→timer_minutes := data→timer_minutes - 1;
        data→timer_seconds := 59;
        data→timer_display→set_text(data→timer_minutes ∥ ":" ∥ data→timer_seconds);
      }
    }
  }
  // Pauses for a second
  time→sleep(1);
  // Resets and displays the timer
  data→timer_minutes := 3;
  data→timer_seconds := 0;
  data→timer_display→set_text(data→timer_minutes ∥ ":0" ∥ data→timer_seconds);
  meta private;
}
var settings_up1 : Sprite {
  // The settings page current smoke up button
}
var settings_down1 : Sprite {
  // The settings page current smoke down button
}

event tap_sprite\u003A_settings_up1(sprite: Sprite, x: Number, y: Number) {
  // Increments the settings current amount
  // Provides user feedback
  phone→vibrate(0.1);
  // Increments the current value
  records→settings_index→at(1)→current→add(1);
  data→settings_current→set_text(records→settings_index→at(1)→current→get→to_string);
  meta private;
}

event tap_sprite\u003A_settings_down1(sprite: Sprite, x: Number, y: Number) {
  // Decrements the settings current amount
  // If the current value is above zero
  if records→settings_index→at(1)→current→get > 0 then {
    // Provides user feedback
    phone→vibrate(0.1);
    // Decrements the current value
    records→settings_index→at(1)→current→add( - 1);
    data→settings_current→set_text(records→settings_index→at(1)→current→get→to_string);
  }
  meta private;
}
var settings_current : Sprite {
  // The settings page current smoke value
}
var settings_target : Sprite {
  // The settings page target smoke value
}
var settings_down2 : Sprite {
  // The settings page target smoke down button
}
var settings_up2 : Sprite {
  // The settings page target smoke up button
}

event tap_sprite\u003A_settings_up2(sprite: Sprite, x: Number, y: Number) {
  // Increments the settings target amount
  // Provides user feedback
  phone→vibrate(0.1);
  // Increments the current value
  records→settings_index→at(1)→target→add(1);
  data→settings_target→set_text(records→settings_index→at(1)→target→get→to_string);
  // Defaults automatic reduction
  code→settings_radio_default;
  meta private;
}

event tap_sprite\u003A_settings_down2(sprite: Sprite, x: Number, y: Number) {
  // Decrements the settings target amount
  // If the current value is above zero
  if records→settings_index→at(1)→target→get > 0 then {
    // Provides user feedback
    phone→vibrate(0.1);
    // Decrements the current value
    records→settings_index→at(1)→target→add( - 1);
    data→settings_target→set_text(records→settings_index→at(1)→target→get→to_string);
    // Defaults automatic reduction
    code→settings_radio_default;
  }
  meta private;
}
var settings_up3 : Sprite {
  // The settings page packet cost up button
}
var settings_down3 : Sprite {
  // The settings page packet cost down button
}
var settings_cost : Sprite {
  // The settings page packrt cost value
}

event tap_sprite\u003A_settings_up3(sprite: Sprite, x: Number, y: Number) {
  // Increments the packet cost amount
  // Provides user feedback
  phone→vibrate(0.1);
  // Increments the current value
  records→settings_index→at(1)→cost_packet→set(math→round_with_precision(records→settings_index→at(1)→cost_packet→get + 0.1, 2));
  $x1 := records→settings_index→at(1)→cost_packet→get→to_string;
  if math→mod(records→settings_index→at(1)→cost_packet→get, 1) = 0 then {
    data→settings_cost→set_text($x1 ∥ ".00");
  }
  else {
    data→settings_cost→set_text($x1 ∥ "0");
  }
  meta private;
}

event tap_sprite\u003A_settings_down3(sprite: Sprite, x: Number, y: Number) {
  // Decrements the packet cost amount
  // If the current value is above zero
  if records→settings_index→at(1)→cost_packet→get > 0 then {
    // Provides user feedback
    phone→vibrate(0.1);
    // Increments the current value
    records→settings_index→at(1)→cost_packet→set(math→round_with_precision(records→settings_index→at(1)→cost_packet→get - 0.1, 2));
    $x1 := records→settings_index→at(1)→cost_packet→get→to_string;
    if math→mod(records→settings_index→at(1)→cost_packet→get, 1) = 0 then {
      data→settings_cost→set_text($x1 ∥ ".00");
    }
    else {
      data→settings_cost→set_text($x1 ∥ "0");
    }
  }
  meta private;
}
var settings_toggle : Sprite {
  // The settings page clear data toggle
}

event swipe_sprite\u003A_settings_toggle(sprite: Sprite, x: Number, y: Number, delta_x: Number, delta_y: Number) {
  if $delta_x > 30 then {
    data→settings_toggle→set_picture(art→toggle_on);
    records→history_store_index→clear;
    records→history_store_index→at(1)→smkdte→set(time→now);
    records→settings_index→at(1)→total_smoked→set(0);
    records→settings_index→at(1)→total_days→set(1);
  }
  meta private;
}

action total_saved_setup() {
  // Calculates the total amount saved
  // Calculates how much has been spent
  $actually_spent := records→settings_index→at(1)→total_smoked→get * (records→settings_index→at(1)→cost_packet→get / 20);
  // Calculates how much would have been spent
  $would_have_spent := (records→settings_index→at(1)→cost_packet→get / 20) * (records→settings_index→at(1)→total_days→get * records→settings_index→at(1)→current→get);
  // Calculates the saving
  $saving := $would_have_spent - $actually_spent;
  // Checks if there is a saving and reports
  if $would_have_spent > $actually_spent then {
    if math→mod($saving, 1) = 0 then {
      data→text_total_saved→set_text("You have saved " ∥ math→round_with_precision($saving, 2) ∥ ".00");
    }
    else {
      if math→ieee_remainder($saving, 0.1) > 0 then {
        data→text_total_saved→set_text("You have saved " ∥ math→round_with_precision($saving, 2));
      }
      else {
        data→text_total_saved→set_text("You have saved " ∥ math→round_with_precision($saving, 2) ∥ "0");
      }
    }
  }
  else {
    data→text_total_saved→set_text("You have saved nothing");
  }
  meta private;
}
var text_total_saved : Sprite {
  // The total amount saved label
}

action target_setup() {
  // Defines the target line on the graph
  $vpos := 540 - (data→graph_scaler * records→settings_index→at(1)→target→get);
  if records→settings_index→at(1)→target→get > 0 then {
    data→target_line→set_color(colors→orange);
    data→target_line→set_z_index(10);
    data→target_line→set_pos(data→board→width / 2, $vpos);
    data→target_label := data→board→create_text(100, 20, 20, "Target: " ∥ records→settings_index→at(1)→target→get→to_string);
    data→target_label→set_color(colors→orange);
    data→target_label→set_z_index(10);
    data→target_label→set_pos(60, $vpos - 20);
  }
  else {
    data→target_line→hide;
    data→target_label→hide;
  }
  meta private;
}
var target_line : Sprite {
  // Sets the target to smoke line
}
var radio_on : Picture {
  is\_resource = true;
  url = "https://sn2files.storage.live.com/y1pmHHOxCfXbX3gTI1YxHlTa0ZVvge-HciYkVfASBMq59vTPOfqbjU5upWjfrEkYq09U8P2SYY6H2Tk02ztO7kiTA/radioon.png.jpg?psid=1";
}
var radio_off : Picture {
  is\_resource = true;
  url = "https://sn2files.storage.live.com/y1pDa1Aju2X9OvREaEZhm80JjAbsOv1wmqodrZFjJPHYsoFH3QXVclaUAj_H-4XatgLKi_mxgO0803H29Dd-wcm0A/radiooff.png.jpg?psid=1";
}
var radio_no : Sprite {
  // The no reduction radio selection
}
var radio_weekly : Sprite {
  // The weekly reduction radio selection
}
var radio_biweekly : Sprite {
  // The biweekly reduction radio selection
}
var radio_monthly : Sprite {
  // The monthly reduction radio selection
}

action settings_radios_initial() {
  // Defines the initial setup of the radio buttons
  data→radio_monthly := data→board_settings→create_picture(art→radio_off);
  data→radio_biweekly := data→board_settings→create_picture(art→radio_off);
  data→radio_weekly := data→board_settings→create_picture(art→radio_off);
  data→radio_no := data→board_settings→create_picture(art→radio_off);
  data→radio_no→set_pos(data→board_settings→width / 5, 430);
  data→radio_weekly→set_pos(data→board_settings→width / 5 * 2, 430);
  data→radio_biweekly→set_pos(data→board_settings→width / 5 * 3, 430);
  data→radio_monthly→set_pos(data→board_settings→width / 5 * 4, 430);
  if records→settings_index→at(1)→reduction→get = 28 then {
    data→radio_monthly→set_picture(art→radio_on);
  }
  else {
    if records→settings_index→at(1)→reduction→get = 14 then {
      data→radio_biweekly→set_picture(art→radio_on);
    }
    else {
      if records→settings_index→at(1)→reduction→get = 7 then {
        data→radio_weekly→set_picture(art→radio_on);
      }
      else {
        data→radio_no→set_picture(art→radio_on);
      }
    }
  }
  meta private;
}

event tap_sprite\u003A_radio_no(sprite: Sprite, x: Number, y: Number) {
  // Sets up no target reduction
  // User feedback
  phone→vibrate(0.1);
  // Sets the button pictures
  data→radio_no→set_picture(art→radio_on);
  data→radio_weekly→set_picture(art→radio_off);
  data→radio_biweekly→set_picture(art→radio_off);
  data→radio_monthly→set_picture(art→radio_off);
  // Set no reduction indication
  records→settings_index→at(1)→reduction→set(1);
  // Set the reduction day counter to onr
  records→settings_index→at(1)→reduction_days→set(1);
  meta private;
}

event tap_sprite\u003A_radio_weekly(sprite: Sprite, x: Number, y: Number) {
  // Sets up weekly target reduction
  // User feedback
  phone→vibrate(0.1);
  // Sets the button pictures
  data→radio_no→set_picture(art→radio_off);
  data→radio_weekly→set_picture(art→radio_on);
  data→radio_biweekly→set_picture(art→radio_off);
  data→radio_monthly→set_picture(art→radio_off);
  // Set weekly reduction indication
  records→settings_index→at(1)→reduction→set(7);
  // Set the reduction day counter to onr
  records→settings_index→at(1)→reduction_days→set(1);
  meta private;
}

event tap_sprite\u003A_radio_biweekly(sprite: Sprite, x: Number, y: Number) {
  // Sets up biweekly target reduction
  // User feedback
  phone→vibrate(0.1);
  // Sets the button pictures
  data→radio_no→set_picture(art→radio_off);
  data→radio_weekly→set_picture(art→radio_off);
  data→radio_biweekly→set_picture(art→radio_on);
  data→radio_monthly→set_picture(art→radio_off);
  // Set biweekly reduction indication
  records→settings_index→at(1)→reduction→set(14);
  // Set the reduction day counter to onr
  records→settings_index→at(1)→reduction_days→set(1);
  meta private;
}

event tap_sprite\u003A_radio_monthly(sprite: Sprite, x: Number, y: Number) {
  // Sets up monthly target reduction
  // User feedback
  phone→vibrate(0.1);
  // Sets the button pictures
  data→radio_no→set_picture(art→radio_off);
  data→radio_weekly→set_picture(art→radio_off);
  data→radio_biweekly→set_picture(art→radio_off);
  data→radio_monthly→set_picture(art→radio_on);
  // Set monthly reduction indication
  records→settings_index→at(1)→reduction→set(28);
  // Set the reduction day counter to onr
  records→settings_index→at(1)→reduction_days→set(1);
  meta private;
}
var target_label : Sprite {
  // The label for the target line
}

action settings_target_automatic(scale: Number, days: Number) {
  // Checks and applies the automatic target reduction
  if math→mod($days, $scale) = 0 then {
    records→settings_index→at(1)→target→set(records→settings_index→at(1)→target→get - 1);
    records→settings_index→at(1)→reduction_days→set(1);
    code→target_setup;
  }
  meta private;
}

action settings_radio_default() {
  // Defaults automatic reduction
  data→radio_no→set_picture(art→radio_on);
  data→radio_weekly→set_picture(art→radio_off);
  data→radio_biweekly→set_picture(art→radio_off);
  data→radio_monthly→set_picture(art→radio_off);
  records→settings_index→at(1)→reduction→set(1);
  records→settings_index→at(1)→reduction_days→set(1);
  meta private;
}

