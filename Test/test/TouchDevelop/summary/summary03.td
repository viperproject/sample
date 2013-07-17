action main () {

   $v := collections→create_number_collection();
   for 0 ≤ i < 5 do {
     $v→add(0);
     $v→set_at(0, $v→at(0) + 1);
   }

}