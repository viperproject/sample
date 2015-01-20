meta version "v2.2,js,ctx,refs,localcloud,unicodemodel,allasync";
meta name "Flip a Virtual Coin!";
meta rootId "EIEsKnHq0FlP8IeZfO4RgMDF";
meta platform "current";
meta parentIds "";
#main
action main() {
  skip;
  if math→random(2) = 1 then {
    art→Tails→post_to_wall;
    "It\'s tails!" →post_to_wall;
  }
  else {
    art→Heads→post_to_wall;
    "                        It\'s heads!" →post_to_wall;
    skip;
  }
  skip;
}
#Heads
var Heads : Picture {
  is\_resource = true;
  url = "http://www.random.org/coins/faces/60-brl/1real/obverse.jpg";
  transient = true;
}
#Tails
var Tails : Picture {
  is\_resource = true;
  url = "http://www.random.org/coins/faces/60-brl/1real/reverse.jpg";
  transient = true;
}