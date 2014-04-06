action main() {
    $p := media→create_picture(10, 10);
    contract->assert($p->width = 10, "must hold");
}