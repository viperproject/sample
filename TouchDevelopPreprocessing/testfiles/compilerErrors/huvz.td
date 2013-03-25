meta version "v2.2,nothing";
meta name "Ar Interpreter Pre-Alpha";
meta icon "files";
meta color "#ffff7518";
// An assembler like interpreter with basic commands add, sub, mul, div, set, mov, print, and call. Version 0.1 made for Windows Phone.
meta platform "current";

action main() {
  data→Index := 0;
  records→Stack_table→clear;
  wall→set_reversed(true);
  data→Registers := records→Register→create_collection;
  code→\_SystemRegisters;
  code→\_GeneralRegisters;
  code→\_GUI;
}
table Register {
  type = "Object";
  fields {
    ID: Number_field
    DATA: Number_field
  }
}
table Stack {
  type = "Table";
  fields {
    com: String_field
    index: Number_field
  }
}

action \_SystemRegisters() {
  data→RegAdd := records→Register→create;
  data→RegAdd→ID→set(0);
  data→RegAdd→DATA→set(0);
  data→RegSub := records→Register→create;
  data→RegSub→ID→set(1);
  data→RegSub→DATA→set(0);
  data→RegMul := records→Register→create;
  data→RegMul→ID→set(2);
  data→RegMul→DATA→set(0);
  data→RegDiv := records→Register→create;
  data→RegDiv→ID→set(3);
  data→RegDiv→DATA→set(0);
  data→Registers→insert_at(0, data→RegAdd);
  data→Registers→insert_at(1, data→RegSub);
  data→Registers→insert_at(2, data→RegMul);
  data→Registers→insert_at(3, data→RegDiv);
  meta private;
}

action add(regA: Register, regB: Register) {
  data→RegAdd→DATA→set($regA→DATA→get + $regB→DATA→get);
  $s := records→Stack_table→add_row;
  $s→index→set(data→Index);
  $s→com→set("add");
  data→Index := data→Index + 1;
  meta private;
}
var RegAdd : Register {
}
var RegSub : Register {
}

action sub(regA: Register, regB: Register) {
  data→RegSub→DATA→set($regA→DATA→get - $regB→DATA→get);
  $s := records→Stack_table→add_row;
  $s→index→set(data→Index);
  $s→com→set("sub");
  data→Index := data→Index + 1;
  meta private;
}
var RegDiv : Register {
}
var RegMul : Register {
}

action print(reg: Register) {
  $reg→DATA→get→post_to_wall;
  $s := records→Stack_table→add_row;
  $s→index→set(data→Index);
  $s→com→set("print");
  data→Index := data→Index + 1;
  meta private;
}

action mul(regA: Register, regB: Register) {
  data→RegMul→DATA→set($regA→DATA→get * $regB→DATA→get);
  $s := records→Stack_table→add_row;
  $s→index→set(data→Index);
  $s→com→set("mul");
  data→Index := data→Index + 1;
  meta private;
}

action div(regA: Register, regB: Register) {
  data→RegDiv→DATA→set($regA→DATA→get / $regB→DATA→get);
  $s := records→Stack_table→add_row;
  $s→index→set(data→Index);
  $s→com→set("div");
  data→Index := data→Index + 1;
  meta private;
}

action \_GeneralRegisters() {
  data→Reg0 := records→Register→create;
  data→Reg0→ID→set(4);
  data→Reg0→DATA→set(0);
  data→Reg1 := records→Register→create;
  data→Reg1→ID→set(5);
  data→Reg1→DATA→set(0);
  data→Reg2 := records→Register→create;
  data→Reg2→ID→set(6);
  data→Reg2→DATA→set(0);
  data→Reg3 := records→Register→create;
  data→Reg3→ID→set(7);
  data→Reg3→DATA→set(0);
  data→Registers→insert_at(4, data→Reg0);
  data→Registers→insert_at(5, data→Reg1);
  data→Registers→insert_at(6, data→Reg2);
  data→Registers→insert_at(7, data→Reg3);
  meta private;
}

action \_Parser(s: String) {
  $strings := $s→split(" ");
  if $strings→at(0)→equals(".add") then {
    code→add(code→getReg($strings→at(1)→to_number), code→getReg($strings→at(2)→to_number));
  }
  if $strings→at(0)→equals(".sub") then {
    code→sub(code→getReg($strings→at(1)→to_number), code→getReg($strings→at(2)→to_number));
  }
  if $strings→at(0)→equals(".mul") then {
    code→mul(code→getReg($strings→at(1)→to_number), code→getReg($strings→at(2)→to_number));
  }
  if $strings→at(0)→equals(".div") then {
    code→div(code→getReg($strings→at(1)→to_number), code→getReg($strings→at(2)→to_number));
  }
  if $strings→at(0)→equals(".mov") then {
    code→mov(code→getReg($strings→at(1)→to_number), code→getReg($strings→at(2)→to_number));
  }
  else {
    if $strings→at(0)→equals(".set") then {
      code→set(code→getReg($strings→at(1)→to_number), $strings→at(2)→to_number);
    }
  }
  if $strings→at(0)→equals(".print") then {
    code→print(code→getReg($strings→at(1)→to_number));
  }
  if $strings→at(0)→equals(".call") then {
    code→call;
  }
  meta private;
}
var Index : Number {
}

action mov(regA: Register, regB: Register) {
  $regA→DATA→set($regB→DATA→get);
  $s := records→Stack_table→add_row;
  $s→index→set(data→Index);
  $s→com→set("mov");
  data→Index := data→Index + 1;
  meta private;
}
var Reg0 : Register {
}
var Reg1 : Register {
}
var Reg2 : Register {
}
var Reg3 : Register {
}

action \_GUI() {
  $s := wall→ask_string("");
  code→\_Parser($s);
  meta private;
}

action getReg(ID: Number) returns reg: Register {
  $reg := records→Register→create;
  foreach r in data→Registers where true do {
    if $r→ID→get = $ID then {
      $reg := $r;
    }
  }
  meta private;
}
var Registers : Register_Collection {
}

event gameloop() {
  code→\_GUI;
  meta private;
}

action set(reg: Register, data1: Number) {
  $reg→DATA→set($data1);
  $s := records→Stack_table→add_row;
  $s→index→set(data→Index);
  $s→com→set("set");
  data→Index := data→Index + 1;
  meta private;
}

action call() {
  foreach s in records→Stack_table where true do {
    $s→post_to_wall;
  }
  meta private;
}

