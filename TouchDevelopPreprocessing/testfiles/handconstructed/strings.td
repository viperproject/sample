meta version "v2.2,nothing";
meta name "SimpleSearch";
meta icon "Binoculars";
meta color "#ff008080";
//
meta platform "current";

action main() {

  $string1 := "a";
  $string2 := "b";

  if $string1->equals("a") then {
    assert->is_true(false);
  }

  if $string2->equals("a") then {
    assert->is_true(false);
  }

}

action priv() {

    assert->is_true(false);

    meta private;

}