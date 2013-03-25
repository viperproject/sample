meta version "v2.2,nothing";
meta name "hmac";
//
meta isLibrary "yes";
meta platform "";
meta import sha1 {
  pub "bqnm"
  usage {
    action calculate(blob: Number_Collection) returns hash: Number_Collection
  }
  resolve collection_util = ♻collection_util with {
  }
  resolve binary_operators = ♻binary_operators with {
  }
}
meta import collection_util {
  pub "ledg"
  usage {
    action allocate(n: Number) returns v: Number_Collection
  }
  resolve binary_operators = ♻binary_operators with {
  }
}
meta import binary_operators {
  pub "vhql"
  usage {
    action xor(a: Number, b: Number) returns r: Number
  }
}

action sha1_hmac(dat: Number_Collection, key: Number_Collection) returns v: Number_Collection {
  if $key→count > 64 then {
    $key := ♻ sha1→calculate($key);
  }
  $ohash := ♻ collection_util→allocate(64);
  $ihash := ♻ collection_util→allocate(64);
  for 0 ≤ i < $key→count do {
    $ohash→set_at($i, $key→at($i));
    $ihash→set_at($i, $key→at($i));
  }
  for 0 ≤ i1 < 64 do {
    $ohash→set_at($i1, ♻ binary_operators→xor($ohash→at($i1), 92));
    $ihash→set_at($i1, ♻ binary_operators→xor($ihash→at($i1), 54));
  }
  $m := collections→create_number_collection;
  $m→add_many($ihash);
  $m→add_many($dat);
  $n := collections→create_number_collection;
  $n→add_many($ohash);
  $n→add_many(♻ sha1→calculate($m));
  $v := ♻ sha1→calculate($n);
}

