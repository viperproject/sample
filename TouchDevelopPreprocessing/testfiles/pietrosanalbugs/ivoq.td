meta version "v2.2,nothing";
meta name "epoch util";
meta icon "Alram";
//
meta isLibrary "yes";
meta platform "";

action get_epoch_time() returns epoch_time: Number {
  $dt := time→now→to_universal_time;
  $days := 0;
  $y := $dt→year - 1970;
  for 0 ≤ i < $y do {
    if code→is_leap_year($i + 1970) then {
      $days := $days + 366;
    }
    else {
      $days := $days + 365;
    }
  }
  $day_of_month := collections→create_number_collection;
  $day_of_month→add(31);
  $day_of_month→add(28);
  $day_of_month→add(31);
  $day_of_month→add(30);
  $day_of_month→add(31);
  $day_of_month→add(30);
  $day_of_month→add(31);
  $day_of_month→add(31);
  $day_of_month→add(30);
  $day_of_month→add(31);
  $day_of_month→add(30);
  $day_of_month→add(31);
  if code→is_leap_year($dt→year) then {
    $day_of_month→set_at(1, 29);
  }
  for 0 ≤ j < $dt→month - 1 do {
    $days := $days + $day_of_month→at($j);
  }
  $days := $days + $dt→day;
  $epoch_time := $days * 86400 + $dt→hour * 3600 + $dt→minute * 60 + $dt→second;
}
var oauth\_credential : String_Map {
}

action is_leap_year(year: Number) returns is_leap_year: Boolean {
  if(((math→mod($year, 4) = 0) and (math→mod($year, 100) ≠ 0)) or (math→mod($year, 400) = 0)) then {
    $is_leap_year := true;
  }
  else {
    $is_leap_year := false;
  }
  meta private;
}

