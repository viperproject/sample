meta version "v2.2,js,ctx";
meta name "item tracker";
meta icon "upbox";
meta color "#ffff7518";
meta platform "current";
// Simple item tracking.

action main() {
  // A little site loaner script.
  code→initialize;
  code→print_stock;
  $loan := wall→ask_boolean("Is this a new loan?", "Use false for returns.");
  if $loan then {
    code→loan;
  }
  else {
    code→@return;
  }
  data→log→post_to_wall;
}
var log : String {
  // keeps track of transactions
}
var items : String_Collection {
}
var loan_times : String_Collection {
  // time of the loan
}
var loan_names : String_Collection {
  // name of the person who loaned
}

action init(item_count: Number) {
  // Initializes the data structures
  data→log := "";
  data→items := code→create_string_collection();
  data→loan_times := code→create_string_collection();
  data→loan_names := code→create_string_collection();
  data→loan_items := code→create_string_collection();
  for 0 ≤ i < $item_count do {
    data→items→add("item " ∥ ($i + 1));
  }
  meta private;
}

action initialize() {
  // Lazy initialization.
  if data→log→is_invalid or data→log→is_empty then {
    code→init(wall→ask_number("How many items?"));
  }
  meta private;
}

action log_text(text: String) {
  // Simple logging method
  $s := time→now ∥ ": " ∥ $text ∥ "\n";
  data→log := $s ∥ data→log;
  meta private;
}

action create_string_collection() returns(r: String_Collection) {
  $r := "" →split(",");
  $r→clear;
  meta private;
}

action is_loaned(item: String) returns(b: Boolean) {
  // Indicates if the item is loaned.
  $b := data→loan_items→contains($item);
  meta private;
}

action loan() {
  // Handles a new loan.
  $loanable := code→get_lonable_items;
  $li := wall→pick_string("Which item so you want to loan?", "These are the items that are in stock.", $loanable);
  $litem := $loanable→at($li);
  $name := wall→ask_string("Please sign the loan agreement and enter your name.");
  data→loan_names→add($name);
  data→loan_times→add(time→now→to_string);
  data→loan_items→add($litem);
  "Enjoy!!!" →post_to_wall;
  wall→set_background(art→success);
  code→log_text("Loaned " ∥ $litem ∥ " to " ∥ $name);
  meta private;
}
var error : Color {
  is\_resource = true;
  url = "#ffffadad";
}

action get_lonable_items() returns(loanable: String_Collection) {
  // Gets the list of item that are in stock.
  $loanable := code→create_string_collection();
  foreach item in data→items
  where not code→is_loaned($item)
  do {
    $loanable→add($item);
  }
  if $loanable→count = 0 then {
    "Sorry, all items are loaned at this moment." →post_to_wall;
    wall→set_background(art→error);
    code→log_text("Loan out of items");
    time→stop;
  }
  meta private;
}
var loan_items : String_Collection {
  // the items currently loaned
}

action print_stock() {
  // Displays the stock status.
  $count := data→loan_times→count;
  $ts := data→loan_times;
  $is := data→loan_items;
  $ns := data→loan_names;
  for 0 ≤ i < $count do {
    ($ts→at($i) ∥ ": " ∥ $ns→at($i) ∥ " : " ∥ $is→at($i))→post_to_wall;
  }
  "loaned" →post_to_wall;
  ("in stock: " ∥ data→items→count - data→loan_items→count ∥ " items")→post_to_wall;
  meta private;
}
var strings : String_Collection {
}

action clear() {
  // Clears all the data
  if wall→ask_boolean("Do you want to delete all the data?", "This operation cannot be undone.") then {
    code→init(0);
  }
}
var success : Color {
  is\_resource = true;
  url = "#ffc7edce";
}

action @return() {
  // Handles an item return.
  $i := wall→pick_string("Which item are you returning?", "", data→loan_items);
  $item := data→loan_items→at($i);
  $name := data→loan_names→at($i);
  data→loan_names→remove_at($i);
  data→loan_items→remove_at($i);
  data→loan_times→remove_at($i);
  code→log_text("Returned " ∥ $item ∥ " by " ∥ $name);
  "Thanks!!!" →post_to_wall;
  wall→set_background(art→success);
  meta private;
}