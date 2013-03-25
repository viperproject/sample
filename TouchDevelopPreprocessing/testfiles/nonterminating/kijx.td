meta version "v2.2,nothing";
meta name "Prime Sieve of Eratosthenes";
meta icon "123";
meta color "#ff007fff";
// This script returns a number collection of primes calculated with the sieve of Eratosthenes algorithm.
// If you execute the script on its own, it will only show primes up to a certain value. This seemingly is a limitation of the wall class and does not affect the actual generation.
// The script has been successfully tested up to the prime 499979 (41538th prime).
// To have the primes 2 and 3 generated, one must set a limit of at least 3 and 4 respectively. All other primes can be generated up to the actual limit itself (i.e. a limit of 11 will generate up to 11 included).
meta isLibrary "yes";
meta platform "current";

action generate_primes(end: Number) returns primes: Number_Collection {
  $primes := collections→create_number_collection;
  $n := collections→create_number_map;
  $i := 2;
  while $i ≤ $end do {
    $n→set_at($i, 1);
    $i := $i + 1;
  }
  $i := 2;
  while $i ≤ math→sqrt($end) do {
    if $n→at($i) = 1 then {
      $j := 0;
      $k := 0;
      while $j ≤ $end do {
        $j := $i * $i + $k * $i;
        $n→set_at($j, 0);
        $k := $k + 1;
      }
    }
    $i := $i + 1;
  }
  $i := 2;
  $l := $n→count;
  while $i ≤ $l do {
    if $n→at($i) = 1 then {
      $primes→add($i);
    }
    $i := $i + 1;
  }
  wall→prompt($primes→max→to_string ∥ " at " ∥ $primes→count);
}

