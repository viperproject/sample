meta version "v2.2,nothing";
meta name "Money";
meta icon "dollar";
meta color "#ffffdf00";
//
meta platform "current";

action main() {
  data→MemberId := 1;
  wall→set_reversed(true);
  "Любимсон, используй кнопку плюсик для заведения операций, а кнопку \"рефреш\", если добавляла новые операции в справочник" →post_to_wall;
  wall→add_button("add", "Создать");
  wall→add_button("refresh", "Обновить");
}

action LoadRefs() {
  if web→is_connected then {
    "Подключение установлено, выполняется обновление справочников..." →post_to_wall;
    $xmlstr := (web→download("Http://money.mhnw.ru/wph/refs.aspx?accountid=1"));
    "Справочники обновлены." →post_to_wall;
    $s := web→xml($xmlstr);
    code→ParseRefs($s);
  }
  else {
    "Отсутствует подключение, проверяем локальное хранилище..." →post_to_wall;
  }
  meta private;
}
table OperationRefs {
  type = "Table";
  fields {
    Id: Number_field
    Caption: String_field
  }
}

action ParseRefs(xml: Xml_Object) {
  foreach xml1 in $xml→children("OperationType") where true do {
    $id := $xml1→attr("Id")→to_number;
    $caption := $xml1→attr("Caption");
    code→InsertOpRef($id, $caption);
  }
  meta private;
}

action InsertOpRef(Id: Number, Caption: String) {
  if code→GetOpRefbyId($Id)→equals("") then {
    $records1 := records→OperationRefs_table→add_row;
    $records1→Id→set($Id);
    $records1→Caption→set($Caption);
  }
  meta private;
}

action GetOpRefbyId(Id: Number) returns Caption: String {
  $Caption := "";
  foreach o in records→OperationRefs_table where true do {
    if $o→Id→get = $Id then {
      $Caption := $o→Caption→get;
    }
  }
  meta private;
}

action CreateOpsCollection() returns Ops: String_Collection {
  $Ops := collections→create_string_collection;
  foreach o in records→OperationRefs_table where true do {
    $Ops→add($o→Caption→get);
  }
  meta private;
}

action GetOpRefbyIndex(i: Number) returns Id: Number {
  $Id := 0;
  $ii := 0;
  foreach o in records→OperationRefs_table where true do {
    if $i = $ii then {
      $Id := $o→Id→get;
    }
    $ii := $ii + 1;
  }
  meta private;
}

action SaveOp(OpTypeId: Number, Sum: Number, Rate: Number, OpDate: DateTime, FromLcl: Boolean) returns Uploaded: Boolean {
  $Uploaded := false;
  if web→is_connected then {
    "Подключение установлено..." →post_to_wall;
    $OpDateStr := $OpDate→day→to_string ∥ "." ∥ $OpDate→month→to_string ∥ "." ∥ $OpDate→year→to_string;
    $OpTimeStr := $OpDate→hour→to_string ∥ ":" ∥ $OpDate→minute→to_string ∥ ":" ∥ $OpDate→second→to_string;
    $reqtxt := "Http://money.mhnw.ru/wph/Saveop.aspx?Optypeid=" ∥ $OpTypeId→to_string ∥ "&Date=" ∥ $OpDateStr ∥ "&Time=" ∥ $OpTimeStr ∥ "&Sum=" ∥ $Sum→to_string ∥ "&Rate=" ∥ $Rate→to_string ∥ "&MemberId=" ∥ data→MemberId→to_string;
    $r := web→create_request($reqtxt);
    $r→send;
    $Uploaded := true;
  }
  else {
    if not $FromLcl then {
      code→SaveOpLcl($OpTypeId, $Sum, $Rate, $OpDate);
    }
  }
  meta private;
}
var CnStr : String {
}

action CreateCnStr(LID: Number, P: String) {
  data→CnStr := "?LId=" ∥ $LID→to_string ∥ "&P=" ∥ $P;
  meta private;
}
table OperationsLcl {
  type = "Table";
  fields {
    OpTId: Number_field
    OpCaption: String_field
    Sum: Number_field
    OpDate: DateTime_field
    OpRate: Number_field
  }
}

action UploadOps() {
  foreach o in records→OperationsLcl_table where true do {
    $res := code→SaveOp($o→OpTId→get, $o→Sum→get, $o→OpRate→get, $o→OpDate→get, true);
    if $res then {
      $o→delete_row;
    }
  }
  meta private;
}

action SaveOpLcl(OpTypeId: Number, Sum: Number, Rate: Number, OpDate: DateTime) {
  $o := records→OperationsLcl_table→add_row;
  $o→OpTId→set($OpTypeId);
  $o→OpDate→set($OpDate);
  $o→Sum→set($Sum);
  $o→OpRate→set($Rate);
  $o→OpCaption→set(code→GetOpRefbyId($OpTypeId));
  ... ;
  meta private;
}

action InputOpForm() {
  code→CreateCnStr(1, "Jgdgvfyy");
  wall→clear;
  "Выбирай операцию" →post_to_wall;
  $opindex := wall→pick_string("", "", code→CreateOpsCollection);
  wall→clear;
  code→GetOpRefbyId(code→GetOpRefbyIndex($opindex))→post_to_wall;
  $sum := wall→ask_number("Сумма");
  $rate := wall→ask_number("Курс");
  if $rate→is_invalid or $rate = 0 then {
    $rate := 1;
  }
  $res := code→SaveOp(code→GetOpRefbyIndex($opindex), $sum, $rate, time→now, false);
  if not $res then {
    "Операция сохранена локально..." →post_to_wall;
  }
  else {
    "Операция сохранена!" →post_to_wall;
  }
  meta private;
}

event gameloop() {
  skip;
  meta private;
}

event tap_wall_Page_Button(item: Page_Button) {
  if $item→text→equals("Создать") then {
    code→InputOpForm;
  }
  else {
    if $item→text→equals("Обновить") then {
      code→LoadRefs;
    }
  }
  meta private;
}
var MemberId : Number {
}

