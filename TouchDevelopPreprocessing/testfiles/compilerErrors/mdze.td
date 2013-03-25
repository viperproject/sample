meta version "v2.2,nothing";
meta name "My Pic Notes";
// Hi fellow developers,
// I built this app which will take a pic, and stores into the recordset that I created along with the date, time and location. When I do a post to wall of the recordset, I get some string only :-( I have declared the column as picture. Please refer to the app screenshot.
//
// Thanks in advance :-)
meta platform "current";

action main() {
  $@table := records→picalbum_table→add_row;
  code→photo($@table);
  records→picalbum_table→post_to_wall;
}
table picalbum {
  type = "Table";
  fields {
    pic: Picture_field
    time: DateTime_field
    loc: Location_field
  }
}

action photo(@table: picalbum) {
  $camera := senses→take_camera_picture;
  $@table→pic→set($camera);
  $@table→time→set(time→now);
  $@table→loc→set(senses→current_location_accurate);
  meta private;
}

