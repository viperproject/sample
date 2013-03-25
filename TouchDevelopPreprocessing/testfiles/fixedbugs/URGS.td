action main() {
  $d := media→create_full_board;
  $d→create_rectangle($d→width, 30);
  $a := colors→dark_gray→darken(0.5);
  $b := $a;
}

