meta version "v2.2,nothing";
meta name "test data";
//
meta platform "current";
meta import data_pic {
  pub "txkm"
  usage {
    action save_string(string: String, title: String)
    action load_string() returns string: String
  }
  resolve base_string = ♻base_string with {
  }
}
meta import base_string {
  pub "lmzr"
  usage {
  }
}

action main() {
  ♻ data_pic→save_string("Final test!!!  \nunicode encoding to color and back\nText to picture and back \nSharing data between apps and users\netc..  \n", "Data as pic");
  ♻ data_pic→load_string→post_to_wall;
}

