meta version "v2.2,nothing";
meta name "Parla con Rocco";
meta icon "Megaphone";
meta color "#ff007FFF";
//
meta platform "current";

action main() {
  code→fill\_table;
  code→create\_space;
  code→create\_space;
  wall→set_foreground(colors→magenta);
  wall→set_background_picture(art→rocco);
  foreach e in records→sentence2Sound_index where true do {
    code→write($e→sentence, 35);
    code→create\_space;
  }
}
var rocco : Picture {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/ztzpnvvs";
}
var sono\_rocco : Sound {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/miyzlwwg";
}
var come\_sei\_fatta : Sound {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/zyipnfwu";
}

action create\_space() {
  media→create_picture(10, 30)→post_to_wall;
  meta private;
}

action write(content: String, fontSize: Number) {
  $tb := wall→create_text_box($content, $fontSize);
  $tb→set_background(colors→transparent);
  $tb→post_to_wall;
  meta private;
}

event tap_wall_TextBox(item: TextBox) {
  records→sentence2Sound_index→at($item→text)→sound→get→play;
  meta private;
}
var di\_dove\_sei : Sound {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/ztzvcoxx";
}

action fill\_table() {
  records→sentence2Sound_index→at("\"Sono Rocco Siffredi\"")→sound→set(art→sono\_rocco);
  records→sentence2Sound_index→at("\"Come sei fatta?\"")→sound→set(art→come\_sei\_fatta);
  records→sentence2Sound_index→at("\"Di dove sei?\"")→sound→set(art→di\_dove\_sei);
  records→sentence2Sound_index→at("\"Mmm\"")→sound→set(art→rocco\_mmm);
  records→sentence2Sound_index→at("\"Ce li hai i peli...\"")→sound→set(art→peli\_sedere);
  records→sentence2Sound_index→at("\"Allora inizi tu o inizio io?\"")→sound→set(art→inizi\_tu\_o\_io);
  records→sentence2Sound_index→at("\"Questo mi fa piacere\"")→sound→set(art→mi\_fa\_piacere);
  records→sentence2Sound_index→at("\"Di solito già dalla voce...\"")→sound→set(art→sei\_portata\_per);
  records→sentence2Sound_index→at("\"Dimmi solo una cosa...\"")→sound→set(art→dimmi\_solo);
  records→sentence2Sound_index→at("\"Come ti piace iniziare?\"")→sound→set(art→come\_ti\_piace);
  meta private;
}
table sentence2Sound {
  type = "Index";
  keys {
    sentence: String
  }
  fields {
    sound: Sound_field
  }
}
var rocco\_mmm : Sound {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/jftqrvzf";
}
var peli\_sedere : Sound {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/kfvrhibq";
}
var inizi\_tu\_o\_io : Sound {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/vemmabje";
}
var mi\_fa\_piacere : Sound {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/grkcqhpl";
}
var sei\_portata\_per : Sound {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/bwueugaj";
}
var dimmi\_solo : Sound {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/cziuqqsh";
}
var come\_ti\_piace : Sound {
  is\_resource = true;
  url = "https://az31353.vo.msecnd.net/pub/ifvkpqsd";
}

