//:: IgnoreFile(/TouchBoost/issue/0/)

// (Manual) test for call graph generation

action main() {
    code->a();
}

action secondPublic() {
    code->c();
}

action a() {
    code->b();

    meta private;
}

action b() {
    code->d();
    code->b();

    meta private;
}

action c() {
    code->d();

    meta private;
}

action d() {
    contract->assert(false, "");

    meta private;
}