var items : String_Collection {
}
var loan_times : String_Collection {
  // time of the loan
}
var loan_names : String_Collection {
  // name of the person who loaned
}

var loan_items : String_Collection {
  // the items currently loaned
}

action main() {
  // Initializes the data structures
  data→items := code→create_string_collection();
  data→loan_times := code→create_string_collection();
  data→loan_names := code→create_string_collection();
  data→loan_items := code→create_string_collection();
  $item_count := wall→ask_number("How many items?");
  for 0 ≤ i < $item_count do {
    data→items→add("item " ∥ ($i + 1));
  }
  $count := data→loan_times→count;
}

action create_string_collection() returns(r: String_Collection) {
  $r := "" →split(",");
  $r→clear;
  meta private;
}