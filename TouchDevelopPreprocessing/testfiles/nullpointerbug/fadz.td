meta version "v2.2,nothing";
meta name "Zodiac Finder";
//
meta platform "current";

action main() {
  wall→set_title("Zodiac Finder");
  wall→set_subtitle("Know your zodiac sign by entering your birth date");
  wall→set_background(colors→brown);
  wall→set_foreground(colors→white);
  $dt := wall→pick_date("What is your birth date?", "YYYY/MM/DD");
  if not $dt→is_invalid then {
    $day := $dt→day;
    $month := $dt→month;
    code→check($day, $month);
  }
  else {
    "Please enter the date in the format: YYYY/MM/DD" →post_to_wall;
    "Invalid date." →post_to_wall;
  }
}

action check(day: Number, month: Number) {
  if not $day→is_invalid or not $month→is_invalid then {
    if $month = 1 then {
      if $day ≥ 1 and $day ≤ 19 then {
        "Your zodiac sign is: CAPRICORN" →post_to_wall;
      }
      else {
        "Your zodiac sign is: AQUARIUS" →post_to_wall;
      }
    }
    else {
      if $month = 2 then {
        if $day ≥ 1 and $day ≤ 18 then {
          "Your zodiac sign is: AQUARIUS" →post_to_wall;
        }
        else {
          "Your zodiac sign is: PISCES" →post_to_wall;
        }
      }
      else {
        if $month = 3 then {
          if $day ≥ 1 and $day ≤ 20 then {
            "Your zodiac sign is: PISCES" →post_to_wall;
          }
          else {
            "Your zodiac sign is: ARIES" →post_to_wall;
          }
        }
        else {
          if $month = 4 then {
            if $day ≥ 1 and $day ≤ 19 then {
              "Your zodiac sign is: ARIES" →post_to_wall;
            }
            else {
              "Your zodiac sign is: TAURUS" →post_to_wall;
            }
          }
          else {
            if $month = 5 then {
              if $day ≥ 1 and $day ≤ 20 then {
                "Your zodiac sign is: TAURUS" →post_to_wall;
              }
              else {
                "Your zodiac sign is: GEMINI" →post_to_wall;
              }
            }
            else {
              if $month = 6 then {
                if $day ≥ 1 and $day ≤ 20 then {
                  "Your zodiac sign is: GEMINI" →post_to_wall;
                }
                else {
                  "Your zodiac sign is: CANCER" →post_to_wall;
                }
              }
              else {
                if $month = 7 then {
                  if $day ≥ 1 and $day ≤ 22 then {
                    "Your zodiac sign is: CANCER" →post_to_wall;
                  }
                  else {
                    "Your zodiac sign is: LEO" →post_to_wall;
                  }
                }
                else {
                  if $month = 8 then {
                    if $day ≥ 1 and $day ≤ 22 then {
                      "Your zodiac sign is: LEO" →post_to_wall;
                    }
                    else {
                      "Your zodiac sign is: VIRGO" →post_to_wall;
                    }
                  }
                  else {
                    if $month = 9 then {
                      if $day ≥ 1 and $day ≤ 22 then {
                        "Your zodiac sign is: VIRGO" →post_to_wall;
                      }
                      else {
                        "Your zodiac sign is: LIBRA" →post_to_wall;
                      }
                    }
                    else {
                      if $month = 10 then {
                        if $day ≥ 1 and $day ≤ 22 then {
                          "Your zodiac sign is: LIBRA" →post_to_wall;
                        }
                        else {
                          "Your zodiac sign is: SCORPIO" →post_to_wall;
                        }
                      }
                      else {
                        if $month = 11 then {
                          if $day ≥ 1 and $day ≤ 21 then {
                            "Your zodiac sign is: SCORPIO" →post_to_wall;
                          }
                          else {
                            "Your zodiac sign is: SAGITTARIUS" →post_to_wall;
                          }
                        }
                        else {
                          if $month = 12 then {
                            if $day ≥ 1 and $day ≤ 21 then {
                              "Your zodiac sign is: SAGITTARIUS" →post_to_wall;
                            }
                            else {
                              "Your zodiac sign is: CAPRICORN" →post_to_wall;
                            }
                          }
                          else {
                            "Invalid date. Please enter the date in the format: YYYY/MM/DD" →post_to_wall;
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  else {
    "Invalid date. Please enter the date in the format: YYYY/MM/DD" →post_to_wall;
  }
  meta private;
}

