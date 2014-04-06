action main() {
  $p := media->create_picture(10,20);
  $w := $p->width;
  contract->assert($w = 10, "must hold");
}