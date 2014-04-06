
//:: ExpectedOutput(backward.counterexample.found)
action card_call() {
  wall→prompt("This action lets you call a number via a calling card.");
  // NOTE: additional is_invalid check because we are using "single execution"
  //       model so data->prefix may be invalid as well.
  if data->prefix->is_invalid then {
    code→reset_calling_card_number();
  }

  if data→prefix→count = 0 then {
    code→reset_calling_card_number();
  }

  $link := phone→choose_phone_number;
  //:: ExpectedOutput(assert.failed)
  $tel := $link→address;
  $tel := $tel→replace("+", "");
  $tel := $tel→replace("-", "");
  $tel := $tel→replace(")", "");
  $tel := $tel→replace("(", "");
  $tel := $tel→replace_regex("X\\d*", "");
  $tel := $tel→trim("");
  $tel := data→prefix ∥ $tel;
  $tel→post_to_wall;
  //:: ExpectedOutput(assert.failed)
  phone→dial_phone_number($tel);
}

var prefix : String {
}

action reset_calling_card_number() {
  data→prefix := wall→ask_string("Please enter the calling card number, including international prefix, and ending with pauses.\nThe number can be cleared with the reset action. ");
  if not data→prefix→starts_with("+") then {
    data→prefix := "+" ∥ data→prefix;
  }
  ("Number: " ∥ data→prefix)→post_to_wall;
}
