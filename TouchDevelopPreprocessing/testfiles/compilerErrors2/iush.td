meta version "v2.2,nothing";
meta name "playing card deck";
meta icon "flowchart";
meta color "#ff008000";
// Reusable library representing one 52-card playing card deck.
meta isLibrary "yes";
meta platform "current";
meta import card {
  pub "bihw"
  usage {
    action new(value: Number) returns c: Json_Object
  }
  resolve suits = ♻suits with {
  }
  resolve definition = ♻definition with {
  }
  resolve core = ♻core resolve array = ♻array resolve collection = ♻collection resolve instance = ♻instance with {
  }
  resolve color_helpers = ♻color_helpers}
meta import suits {
  pub "qzig"
  usage {
  }
}
meta import core {
  pub "dnoo"
  usage {
  }
}
meta import array {
  pub "bzjc"
  usage {
    action remove_at(@in: Json_Object, at: Number) returns out: Json_Object
    action from_strings(strings: String_Collection) returns json: Json_Object
  }
  resolve core = ♻core with {
  }
}
meta import collection {
  pub "tizp"
  usage {
  }
  resolve core = ♻core with {
  }
}
meta import definition {
  pub "txih"
  usage {
  }
  resolve core = ♻core resolve array = ♻array with {
  }
  resolve collection = ♻collection with {
  }
}
meta import color_helpers {
  pub "wszv"
  usage {
  }
}
meta import instance {
  pub "sbog"
  usage {
  }
  resolve core = ♻core with {
  }
  resolve array = ♻array resolve color_helpers = ♻color_helpers with {
  }
  resolve collection = ♻collection with {
  }
}

action shuffle() {
  $left := collections→create_string_collection;
  for 0 ≤ i < 52 do {
    $left→add($i→to_string);
  }
  data→left := ♻ array→from_strings($left);
}

action deal() returns new_card: Json_Object {
  $c := data→left→count;
  if $c = 0 then {
    code→shuffle;
    $c := 52;
  }
  $x := math→random($c);
  $val := data→left→at($x)→to_number;
  data→left := ♻ array→remove_at(data→left, $x);
  $new_card := ♻ card→new($val);
}

action count() returns i: Number {
  $i := data→left→count;
}
var left : Json_Object {
}

