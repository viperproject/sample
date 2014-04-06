// NOTE: Test case only deals with the first error. Our backward analysis cannot
//  infer that json-fields must/must not be there

// A first attempt at handling json data. Queries isitup.org json API checking if a website is working or not and return its IP address if it is.
//:: ExpectedOutput(backward.counterexample.found)
action isitup() {
  // Utilize isitup.org json API to check sites are working.
  // A first attempt at handling json data.
  //
  // Site to check.
  $site := wall→ask_string("Site to check.");
  //
  // Build the query url.
  $url := "http://isitup.org/" ∥ web→url_encode($site) ∥ ".json";
  //
  // Download the result and parse it into a json data structure
  $downloaded := web→download($url);
  //:: ExpectedOutput(assert.failed)
  $json := web→json($downloaded);
  // Fetch required fields from the json object.
  //:: ExpectedOutput(assert.failed)
  $sc := $json→field("status_code")→to_number;
  //:: ExpectedOutput(assert.failed)
  $dom := $json→field("domain")→to_string;
  if $sc = 1 then {
    // If site is up then display some info.
    //:: ExpectedOutput(assert.failed)
    $ip := $json→field("response_ip")→to_string;
    //:: ExpectedOutput(assert.failed)
    $rc := $json→field("response_code")→to_string;
    wall→prompt($dom ∥ " (IP: " ∥ $ip ∥ ")" ∥ "\nIs working.");
  }
  else {
    // If site is down then display.
    wall→prompt($dom ∥ " Is not working.");
  }

  meta private;
}

action go() {
  // Lets call it, then we can add other functionality/routines later.
  code→isitup;
  $b := wall→ask_boolean("Check another?", "");
  if $b then {
    wall→clear;
    code→go;
  }
}
