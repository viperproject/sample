
action main() {

  $string1 := "a";
  $string2 := "b";

  $string3 := $string1 ∥ $string2;

  if not $string3->equals("ab") then {
    contract->assert(false,"");
  }

}