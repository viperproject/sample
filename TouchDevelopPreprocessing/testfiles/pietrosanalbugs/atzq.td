meta version "v2.2,nothing";
meta name "My£xpense Note";
meta icon "pound";
meta color "#ffff7518";
// This script will note your payment / income into the tables. It's a simple script to show how records (table) work.
meta platform "current";

action main() {
  wall→clear;
  code→firstMenu;
}
var income : String {
}
var summary : String {
}
var note : String {
}

event tap_wall_String(item: String) {
  wall→clear;
  if $item→equals("Food payment") then {
    code→insertRecord("Food payment");
  }
  else {
    if $item→equals("Misc payment") then {
      code→insertRecord("Misc payment");
    }
    else {
      if $item→equals("Income") then {
        code→insertRecord("Income");
      }
      else {
        if $item→equals("Create summary record") then {
          wall→clear;
          $starttime := wall→pick_date("Select start time", "pick a date");
          wall→clear;
          $endtime := wall→pick_date("Select end time", "pick a date");
          code→summarizer($starttime, $endtime);
        }
        else {
          if $item→equals("View all transactions") then {
            records→transactions_table→post_to_wall;
            $back := wall→ask_boolean("Back to main menu?", "Yes - go to main menu");
            if $back then {
              code→main;
            }
          }
          else {
            records→savedSummary_table→post_to_wall;
            $back1 := wall→ask_boolean("Back to main menu?", "Yes - go to main menu");
            if $back1 then {
              code→main;
            }
          }
        }
      }
    }
  }
  meta private;
}
var foodPaym : String {
}
var miscPaym : String {
}

action firstMenu() {
  data→foodPaym := "Food payment";
  data→miscPaym := "Misc payment";
  data→income := "Income";
  data→summary := "Create summary record";
  data→vSummary := "View summary";
  data→vTxn := "View all transactions";
  $linesep := "-------------------------";
  // Show list on the wall
  $linesep→post_to_wall;
  data→vSummary→post_to_wall;
  $linesep→post_to_wall;
  data→summary→post_to_wall;
  $linesep→post_to_wall;
  data→vTxn→post_to_wall;
  $linesep→post_to_wall;
  data→income→post_to_wall;
  $linesep→post_to_wall;
  data→miscPaym→post_to_wall;
  $linesep→post_to_wall;
  data→foodPaym→post_to_wall;
  $linesep→post_to_wall;
  "Please select (tap) the following transaction type :" →post_to_wall;
  meta private;
}

action insertRecord(recordType: String) {
  wall→clear;
  $amount := wall→ask_number("Insert amount of " ∥ $recordType);
  $note := wall→ask_string("Insert note for this transaction");
  $txn := records→transactions_table→add_row;
  $txn→timestamp→set(time→now);
  $txn→amount→set($amount);
  $txn→type→set($recordType);
  $txn→note→set($note);
  wall→clear;
  $result := $amount→to_string;
  $result := $result ∥ " THB of " ∥ $recordType ∥ " saved!";
  $result→post_to_wall;
  code→main;
  meta private;
}
table transactions {
  type = "Table";
  fields {
    timestamp: DateTime_field
    amount: Number_field
    type: String_field
    note: String_field
  }
}
var txnRow : transactions {
}

action summarizer(start: DateTime, end: DateTime) {
  $foodsum := 0;
  $miscsum := 0;
  $incosum := 0;
  foreach t in records→transactions_table where $start→less_or_equals($t→timestamp→get) and $end→greater_or_equal($t→timestamp→get) do {
    if $t→type→get→equals("Food payment") then {
      $foodsum := $foodsum + $t→amount→get;
    }
    else {
      if $t→type→get→equals("Misc payment") then {
        $miscsum := $miscsum + $t→amount→get;
      }
      else {
        $incosum := $incosum + $t→amount→get;
      }
    }
  }
  wall→clear;
  $foodsumres := $foodsum→to_string;
  $miscsumres := $miscsum→to_string;
  $incosumres := $incosum→to_string;
  $foodsumres := "Total food payment : " ∥ $foodsumres;
  $miscsumres := "Total Misc payment : " ∥ $miscsumres;
  $incosumres := "Total income : " ∥ $incosumres;
  $foodsumres→post_to_wall;
  $miscsumres→post_to_wall;
  $incosumres→post_to_wall;
  // Write data to savedSummary
  $savesum := records→savedSummary_table→add_row;
  $savesum→qryStart→set($start);
  $savesum→qryEnd→set($end);
  $savesum→totalFoodpay→set($foodsum);
  $savesum→totalMiscpay→set($miscsum);
  $savesum→totalIncome→set($incosum);
  if wall→ask_boolean("Back to menu?", "Yes - back to main menu\nNo - do nothing") then {
    code→main;
  }
  meta private;
}
table savedSummary {
  // The summary records save here
  type = "Table";
  fields {
    qryStart: DateTime_field
    qryEnd: DateTime_field
    totalFoodpay: Number_field
    totalMiscpay: Number_field
    totalIncome: Number_field
  }
}
var vSummary : String {
}
var vTxn : String {
}

