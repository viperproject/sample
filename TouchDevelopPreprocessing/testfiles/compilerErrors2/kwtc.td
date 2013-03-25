meta version "v2.2,nothing";
meta name "collection util";
//
meta isLibrary "yes";
meta platform "current";

action allocate(n: Number) returns v: Number_Collection {
  $v := collections→create_number_collection;
  for 0 ≤ i < $n do {
    $v→add(0);
  }
}

action from_string(s: String) returns v: Number_Collection {
  $@table := "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
  $v := collections→create_number_collection;
  $s := web→url_encode($s);
  $x2 := 0;
  while $x2 < $s→count do {
    $c := $s→at($x2);
    if $c→compare("!") = 0 then {
      $v→add(33);
    }
    else {
      if $c→compare("@") = 0 then {
        $v→add(64);
      }
      else {
        if $c→compare("#") = 0 then {
          $v→add(35);
        }
        else {
          if $c→compare("$") = 0 then {
            $v→add(36);
          }
          else {
            if $c→compare("^") = 0 then {
              $v→add(94);
            }
            else {
              if $c→compare("*") = 0 then {
                $v→add(42);
              }
              else {
                if $c→compare("(") = 0 then {
                  $v→add(40);
                }
                else {
                  if $c→compare(")") = 0 then {
                    $v→add(41);
                  }
                  else {
                    if $c→compare("_") = 0 then {
                      $v→add(95);
                    }
                    else {
                      if $c→compare("+") = 0 then {
                        $v→add(43);
                      }
                      else {
                        if $c→compare(":") = 0 then {
                          $v→add(58);
                        }
                        else {
                          if $c→compare("?") = 0 then {
                            $v→add(63);
                          }
                          else {
                            if $c→compare("-") = 0 then {
                              $v→add(45);
                            }
                            else {
                              if $c→compare("=") = 0 then {
                                $v→add(61);
                              }
                              else {
                                if $c→compare(";") = 0 then {
                                  $v→add(59);
                                }
                                else {
                                  if $c→compare("\'") = 0 then {
                                    $v→add(39);
                                  }
                                  else {
                                    if $c→compare(",") = 0 then {
                                      $v→add(44);
                                    }
                                    else {
                                      if $c→compare(".") = 0 then {
                                        $v→add(46);
                                      }
                                      else {
                                        if $c→compare("/") = 0 then {
                                          $v→add(47);
                                        }
                                        else {
                                          if $c→compare("~") = 0 then {
                                            $v→add(126);
                                          }
                                          else {
                                            if $c→compare("&") = 0 then {
                                              $v→add(38);
                                            }
                                            else {
                                              if $c→compare("%") = 0 then {
                                                $s2 := $s→substring($x2 + 1, 2);
                                                $v→add(code→hex_to_int($s2));
                                                $x2 := $x2 + 2;
                                              }
                                              else {
                                                $x := $@table→index_of($c, 0);
                                                if $x < 10 then {
                                                  $v→add(48 + $x);
                                                }
                                                else {
                                                  if $x < 36 then {
                                                    $v→add(55 + $x);
                                                  }
                                                  else {
                                                    $v→add(61 + $x);
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
    $x2 := $x2 + 1;
  }
}

action hex_to_int(s: String) returns r: Number {
  $@table := "0123456789abcdef";
  $r := 0;
  foreach e in $s where true do {
    $r := $r * 16 + $@table→index_of($e→to_lower_case, 0);
  }
  meta private;
}

action to_hex_string(v: Number_Collection) returns s: String {
  $s := "";
  foreach e in $v where true do {
    $s := $s ∥ code→int_to_hex($e);
  }
}

action int_to_hex(p: Number) returns s: String {
  $@table := "0123456789abcdef";
  $s := $@table→at(math→floor($p / 16)) ∥ $@table→at(math→mod($p, 16));
  meta private;
}

action to_string(v: Number_Collection) returns s: String {
  $s := "";
  foreach e in $v where true do {
    $s := $s ∥ $e→to_character;
  }
}

