meta version "v2.2,nothing";
meta name "Volkskrant";
meta icon "journal";
meta color "#ff007fff";
// Volkskrant
meta showAd "yes";
meta platform "current";

action main() {
  "TouchDevelop is cool!" →post_to_wall;
}

action go() {
  skip;
  meta private;
}
table Nieuws {
  // nieuws van vandaag
  type = "Object";
  fields {
    @40: Number_field
  }
}

