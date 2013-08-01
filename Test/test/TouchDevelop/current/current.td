meta version "v2.2,js,ctx";
meta name "my script 26";
meta platform "current";

action main() {
  data→places := collections→create_place_collection;
  data→places→add(social→create_place("", locations→create_location(0, 0)));
  contract→assert(data→places→count < 2, "test");
}
var places : Place_Collection {}