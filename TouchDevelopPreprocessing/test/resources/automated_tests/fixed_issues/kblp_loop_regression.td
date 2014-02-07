// Should not produce a BOTTOM warning (TODO: maybe add bottom annotations for testing)

action main() {
  code→show_Map(1);
  code→show_Map(0);


}

action show_Map(zoomin: Number) {
   if $zoomin = 1 then {
    for 0 ≤ i1 < 7 do {
      "hello world"->post_to_wall();
    }
  }

  meta private;
}
