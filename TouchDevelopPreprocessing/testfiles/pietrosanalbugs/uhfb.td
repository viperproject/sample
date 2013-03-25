meta version "v2.2,nothing";
meta name "13Ds";
//
meta platform "current";

action main() {
  "Hello world!" →post_to_wall;
}

action get_SEC_13Ds(CIKorTicker: String) {
  $s4 := "HTTP://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=" ∥ $CIKorTicker ∥ "&type=&dateb=&owner=include&start=0&count=100&output=atom";
  $NS := "{http://www.w3.org/2005/Atom}";
  skip;
  $s1 := web→download($s4);
  $xml1 := web→xml($s1);
  foreach e in $xml1 where $e→name→equals("entry") do {
    foreach e2 in $e where $e2→name→equals("category") where $e2→attr("term")→contains("13D") do {
      foreach e3 in $e where $e3→name→equals("link") do {
        skip;
        $s := $e3→attr("href");
        $s5 := web→download($s);
        $x := 0;
        $x := $s5→index_of("\"companyName\">", 0);
        while $x ≥ 0 do {
          $s5 := $s5→substring($x + 14, $s5→count - $x - 14);
          $x2 := $s5→index_of("<", 0);
          $s5→substring(0, $x2)→post_to_wall;
          $x := $s5→index_of("\"companyName\">", 0);
        }
        web→link_url("13D", $s)→post_to_wall;
      }
    }
  }
}

