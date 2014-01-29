meta version "v2.2,nothing";
meta name "SimpleSearch";
meta icon "Binoculars";
meta color "#ff008080";
//
meta platform "current";

action main() {

  $string1 := "a";
  $string2 := "b";

  if $string1->equals("b") then {
    contract->assert(false,"");
  }

  if $string2->equals("a") then {
    contract->assert(false,"");
  }

}

action priv() {

    contract->assert(false,"");

    meta private;

}