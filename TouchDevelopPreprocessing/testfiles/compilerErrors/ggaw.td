meta version "v2.2,nothing";
meta name "Smoking Monitor";
meta icon "delete";
meta color "#ff007fff";
// Assists you to monitor your smoking to help you quit.
//
// Includes:
// A counter on the tile.
// A timer to try to outlast your craving.
// Graphical amount smoked over 28 days.
// Time since last smoked.
//
// Limitations:
// On a new day the counter on the tile will display the previous days count until you open the app.
// The time that you last smoked updates when you open the app or smoke.
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
  // Sets the counter on the main tile
  data→main_tile→set_counter(records→history_store_index→at(1)→smoked→get);
  // Puts every thing on the wall
  data→board→post_to_wall;
  // Adds a page button to reset data
  wall→add_button("cancel", "Reset");
}
var main_tile : Tile {
  // The main tile
  readonly = true;
}
var timer_desciption : Sprite {
  // Text describing the timer
}
var timer_display : Sprite {
  // Displays a 3 minute timer
}
var board : Board {
  // The display space
}
var text_title : Sprite {
  // Board title
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
  // Set the last time smoked
  records→history_store_index→at(1)→last_smoked→set(time→now);
  // Adds one to the main tile counter
  data→main_tile→set_counter(records→history_store_index→at(1)→smoked→get);
  // Displays the amended text on the main screen
  data→text_daily_count→set_text("You\'ve smoked " ∥ records→history_store_index→at(1)→smoked→get ∥ " today");
  // Calls for the graph to be updated
  code→graph_update;
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
var text_daily_count : Sprite {
  // Text displaying daily total
}

action new_day_setup() {
  // Moves all the previous days values down one in the store and sets the new day to zero.
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
  meta private;
}

event tap_wall_Page_Button(item: Page_Button) {
  if $item→text→equals("Reset") then {
    wall→push_new_page;
    wall→set_reversed(true);
    wall→create_text_box("Are you sure you want to clear all the stored days?", 36)→post_to_wall;
    wall→add_button("check", "Delete");
    wall→add_button("cancel", "Keep all");
  }
  else {
    if $item→text→equals("Keep all") then {
      wall→pop_page;
    }
    else {
      records→history_store_index→clear;
      records→history_store_index→at(1)→smoked→set(0);
      records→history_store_index→at(1)→smkdte→set(time→now);
      wall→pop_page;
    }
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
  data→text_title := data→board→create_text(400, 40, 50, "Smoking Monitor");
  data→text_title→set_pos((data→board→width / 2), 20);
  // Defines the text for smoked today
  data→text_daily_count := data→board→create_text(400, 20, 36, "You\'ve smoked " ∥ records→history_store_index→at(1)→smoked→get ∥ " today");
  data→text_daily_count→set_color(colors→orange);
  data→text_daily_count→set_pos((data→board→width / 2), 80);
  // Defines the text describing the timer
  data→timer_desciption := data→board→create_text(480, 40, 20, "Apparently cravings last for 3 minutes.\nIf you want to try to outlast it start the timer.");
  data→timer_desciption→set_pos((data→board→width / 2), 150);
  // The initial timer display
  data→timer_display := data→board→create_text(100, 20, 50, data→timer_minutes ∥ ":0" ∥ data→timer_seconds);
  data→timer_display→set_pos((data→board→width / 3), 195);
  // Defines the start timer button
  data→timer_start := data→board→create_text(100, 30, 30, "Start timer");
  data→timer_start→set_pos(((data→board→width / 3) * 2), 215);
  // Defines the smoke now display
  data→text_smoke_now := data→board→create_text(300, 20, 60, "Smoke Now");
  data→text_smoke_now→set_color(colors→orange);
  data→text_smoke_now→set_pos((data→board→width / 2), 555);
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
  data→graph_line := data→board→create_rectangle(data→board→width / 2 + 215, 2);
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
  for 0 ≤ i < 30 do {
    if records→history_store_index→at($i)→smoked→get > $j then {
      $j := records→history_store_index→at($i)→smoked→get;
    }
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
  // Sets the counter on the main tile
  data→main_tile→set_counter(records→history_store_index→at(1)→smoked→get);
  // Puts every thing on the wall
  data→board→post_to_wall;
  meta private;
}
var graph_hpostion : Number {
  // The horizontal position at which the graph layout is based
}
var timer_start : Sprite {
  // The start timer button
}

event tap_sprite\u003A_timer_start(sprite: Sprite, x: Number, y: Number) {
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
var text_last_smoked : Sprite {
  // The date/time last smoked
}
var dtg_last_smoked : DateTime {
  // The date/time last smoked
}

action last_smoked_setup() {
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
  // Sets position & colour of last smoked text
  data→text_last_smoked→set_color(colors→orange);
  data→text_last_smoked→set_pos(data→board→width / 2, 265);
  meta private;
}

action test() {
  for 0 ≤ i < 28 do {
    records→history_store_index→at($i + 1)→smoked→set(math→random(18));
    records→history_store_index→at($i + 1)→smkdte→set(time→now→add_days( - 1));
    records→history_store_index→at($i + 1)→last_smoked→set(time→now→add_days( - 1));
  }
  meta private;
}

