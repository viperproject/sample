meta version "v2.2,nothing";
meta name "My Pic Notes";
//
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

