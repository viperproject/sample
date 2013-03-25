meta version "v2.2,nothing";
meta name "Gel analyzer";
//
meta platform "current";

action main() {
  "TouchDevelop is cool!" →post_to_wall;
  code→go;
}

action go() {
  data→gelpic := media→choose_picture;
  data→hor\u002Dint := collections→create_number_collection;
  for 0 ≤ i < data→gelpic→width do {
    data→hor\u002Dint→add(0);
    for 0 ≤ i1 < data→gelpic→height do {
      data→hor\u002Dint→insert_at($i, data→hor\u002Dint→at($i) + data→gelpic→pixel($i, $i1)→brightness);
    }
  }
  data→hor\u002Dint→post_to_wall;
  meta private;
}
var pic : Picture {
  is\_resource = true;
}
var gelpic : Picture {
}
var hor\u002Dint : Number_Collection {
}

