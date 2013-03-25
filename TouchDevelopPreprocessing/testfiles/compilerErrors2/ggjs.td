meta version "v2.2,nothing";
meta name "classlib";
meta icon "flowchart";
meta color "#ff004b49";
// This script can be used as the basis for any script that needs to manipulate class-like objects.  If you improve it, post a new version.
meta platform "current";

action main() {
  "This script can be used as the basis for any script that needs to manipulate class-like objects.  If you improve it, post a new version." →post_to_wall;
  time→sleep(3);
  wall→clear;
  code→test;
  web;
}

action arrayInsertJson(@in: Json_Object, at: Number, value: Json_Object) returns out: Json_Object {
  $out := code→arrayInsertValue($@in, $at, code→serializeJson($value));
  meta private;
}

action arrayInsertString(@in: Json_Object, at: Number, value: String) returns out: Json_Object {
  $out := code→arrayInsertValue($@in, $at, code→serializeString($value));
  meta private;
}

action arrayInsertValue(@in: Json_Object, at: Number, value: String) returns out: Json_Object {
  $strings := code→newStringCollection;
  $strings→add("[");
  $array := code→newStringCollection;
  for 0 ≤ i < $@in→count do {
    if $i = $at then {
      $array→add($value);
    }
    $array→add(code→serializeJson($@in→at($i)));
  }
  if $at = $@in→count then {
    $array→add($value);
  }
  $strings→add($array→join(","));
  $strings→add("]");
  $out := web→json($strings→join(""));
  meta private;
}

action arrayRemoveAt(@in: Json_Object, at: Number) returns out: Json_Object {
  $strings := code→newStringCollection;
  $strings→add("[");
  $array := code→newStringCollection;
  for 0 ≤ i < $@in→count do {
    if $i ≠ $at then {
      $array→add(code→serializeJson($@in→at($i)));
    }
  }
  $strings→add($array→join(","));
  $strings→add("]");
  $out := web→json($strings→join(""));
  meta private;
}

action newArray() returns json: Json_Object {
  $json := web→json("[]");
  meta private;
}

action newObject() returns json: Json_Object {
  $json := web→json("{}");
  meta private;
}

action newStringCollection() returns strings: String_Collection {
  $strings := "" →split(",");
  $strings→clear;
  meta private;
}

action objectAddJson(@in: Json_Object, name: String, value: Json_Object) returns out: Json_Object {
  $out := code→objectAddValue($@in, $name, code→serializeJson($value));
  meta private;
}

action objectAddString(@in: Json_Object, name: String, value: String) returns out: Json_Object {
  $out := code→objectAddValue($@in, $name, code→serializeString($value));
  meta private;
}

action objectAddValue(@in: Json_Object, name: String, value: String) returns out: Json_Object {
  $strings := code→newStringCollection;
  $strings→add("{");
  $object := code→newStringCollection;
  for 0 ≤ k < $@in→keys→count do {
    $object→add($@in→keys→at($k) ∥ ":" ∥ code→serializeJson($@in→field($@in→keys→at($k))));
  }
  $object→add($name ∥ ":" ∥ $value);
  $strings→add($object→join(","));
  $strings→add("}");
  $out := web→json($strings→join(""));
  meta private;
}

action objectRemoveByName(@in: Json_Object, name: String) returns out: Json_Object {
  $strings := code→newStringCollection;
  $strings→add("{");
  $object := code→newStringCollection;
  for 0 ≤ k < $@in→keys→count do {
    $n := $@in→keys→at($k);
    if not $n→equals($name) then {
      $object→add($n ∥ ":" ∥ code→serializeJson($@in→field($@in→keys→at($k))));
    }
  }
  $strings→add($object→join(","));
  $strings→add("}");
  $out := web→json($strings→join(""));
  meta private;
}

action objectRename(@in: Json_Object, oldName: String, newName: String) returns out: Json_Object {
  $strings := code→newStringCollection;
  $strings→add("{");
  $object := code→newStringCollection;
  for 0 ≤ k < $@in→keys→count do {
    $n := $@in→keys→at($k);
    $v := code→serializeJson($@in→field($n));
    if $n→equals($oldName) then {
      $object→add($newName ∥ ":" ∥ $v);
    }
    else {
      $object→add($n ∥ ":" ∥ $v);
    }
  }
  $strings→add($object→join(","));
  $strings→add("}");
  $out := web→json($strings→join(""));
  meta private;
}

action serializeArray(json: Json_Object) returns s: String {
  $strings := code→newStringCollection;
  $strings→add("[");
  $array := code→newStringCollection;
  for 0 ≤ i < $json→count do {
    $array→add(code→serializeJson($json→at($i)));
  }
  $strings→add($array→join(","));
  $strings→add("]");
  $s := $strings→join("");
  meta private;
}

action serializeJson(json: Json_Object) returns s: String {
  $s := "null";
  if not $json→is_invalid then {
    if $json→kind→equals("string") then {
      $s := code→serializeString($json→to_string);
    }
    else {
      if $json→kind→equals("object") then {
        $s := code→serializeObject($json);
      }
      else {
        if $json→kind→equals("array") then {
          $s := code→serializeArray($json);
        }
        else {
          if $json→kind→equals("number") then {
            $s := $json→to_number ∥ "";
          }
          else {
            $s := $json→to_boolean ∥ "";
          }
        }
      }
    }
  }
  meta private;
}

action serializeObject(json: Json_Object) returns s: String {
  $strings := code→newStringCollection;
  $strings→add("{");
  $object := code→newStringCollection;
  for 0 ≤ k < $json→keys→count do {
    $object→add($json→keys→at($k) ∥ ":" ∥ code→serializeJson($json→field($json→keys→at($k))));
  }
  $strings→add($object→join(","));
  $strings→add("}");
  $s := $strings→join("");
  meta private;
}

action serializeString(value: String) returns s: String {
  $strings := code→newStringCollection;
  $strings→add("\"");
  foreach char in $value where true do {
    $x := $char→to_unicode;
    if $x < 32 or $x = 34 or $x = 47 or $x = 94 then {
      $strings→add("\\u" ∥ code→toHex($x));
    }
    else {
      $strings→add($char);
    }
  }
  $strings→add("\"");
  $s := $strings→join("");
  meta private;
}

action toHex(x: Number) returns s: String {
  $strings := "" →split(",");
  $strings→clear;
  while $x > 0 or $strings→count < 4 do {
    $byte := math→mod($x, 16);
    $x := ($x - $byte) / 16;
    if $byte < 10 then {
      $strings→add($byte→to_string);
    }
    else {
      $strings→add(($byte + 55)→to_character);
    }
  }
  $strings→@reverse;
  $s := $strings→join("");
  meta private;
}

action arrayAddValue(@in: Json_Object, value: String) returns out: Json_Object {
  $out := code→arrayInsertValue($@in, $@in→count, $value);
  meta private;
}

action arrayAddString(@in: Json_Object, value: String) returns out: Json_Object {
  $out := code→arrayAddValue($@in, code→serializeString($value));
  meta private;
}

action arrayAddJson(@in: Json_Object, value: Json_Object) returns out: Json_Object {
  $out := code→arrayAddValue($@in, code→serializeJson($value));
  meta private;
}
var classData : Json_Object {
}

action classlibInit() {
  data→classData := code→newObject;
  data→classData := code→objectAddString(data→classData, "type", "classlib");
  data→classData := code→objectAddString(data→classData, "version", "0.6");
  data→classData := code→objectAddJson(data→classData, "definitions", code→newArray());
  meta private;
}

action arrayEditValue(@in: Json_Object, at: Number, value: String) returns out: Json_Object {
  $strings := code→newStringCollection;
  $strings→add("[");
  $array := code→newStringCollection;
  for 0 ≤ i < $@in→count do {
    if $i = $at then {
      $array→add($value);
    }
    else {
      $array→add(code→serializeJson($@in→at($i)));
    }
  }
  $strings→add($array→join(","));
  $strings→add("]");
  $out := web→json($strings→join(""));
  meta private;
}

action arrayEditString(@in: Json_Object, at: Number, value: String) returns out: Json_Object {
  $out := code→arrayEditValue($@in, $at, code→serializeString($value));
  meta private;
}

action arrayEditJson(@in: Json_Object, at: Number, value: Json_Object) returns out: Json_Object {
  $out := code→arrayEditValue($@in, $at, code→serializeJson($value));
  meta private;
}

action objectEditValue(@in: Json_Object, name: String, value: String) returns out: Json_Object {
  $strings := code→newStringCollection;
  $strings→add("{");
  $object := code→newStringCollection;
  for 0 ≤ k < $@in→keys→count do {
    $n := $@in→keys→at($k);
    $v := code→serializeJson($@in→field($n));
    if $n→equals($name) then {
      $object→add($n ∥ ":" ∥ $value);
    }
    else {
      $object→add($n ∥ ":" ∥ $v);
    }
  }
  $strings→add($object→join(","));
  $strings→add("}");
  $out := web→json($strings→join(""));
  meta private;
}

action objectEditString(@in: Json_Object, name: String, value: String) returns out: Json_Object {
  $out := code→objectEditValue($@in, $name, code→serializeString($value));
  meta private;
}

action objectEditJson(@in: Json_Object, name: String, value: Json_Object) returns out: Json_Object {
  $out := code→objectEditValue($@in, $name, code→serializeJson($value));
  meta private;
}

action classlibSetDefs(defs: Json_Object) {
  data→classData := code→objectEditJson(data→classData, "definitions", $defs);
  meta private;
}

action defineClass(name: String) {
  code→defineInheritedClass($name, "");
  meta private;
}

action defineProperty(class: String, propertyName: String, propertyType: String) {
  $prop := code→newObject;
  $prop := code→objectAddString($prop, "type", $propertyType);
  $prop := code→objectAddString($prop, "name", $propertyName);
  $def := code→classlibGetDef($class);
  $props := code→arrayAddJson($def→field("properties"), $prop);
  $def := code→objectEditJson($def, "properties", $props);
  code→classlibSetDef($def);
  meta private;
}

action classlibGetDefs() returns defs: Json_Object {
  $defs := data→classData→field("definitions");
  meta private;
}

action test() {
  code→classlibInit;
  code→defineClass("pet");
  code→defineProperty("pet", "name", "string");
  code→defineProperty("pet", "age", "number");
  code→defineMethod("pet", "post", "null");
  code→defineParameter("pet", "post", "message", "string");
  code→defineMethod("pet", "incrementAge", "number");
  code→defineInheritedClass("cat", "pet");
  code→defineMethod("cat", "meow", "null");
  code→defineInheritedClass("dog", "pet");
  code→defineMethod("dog", "bark", "null");
  $goldfish := code→newClass("pet");
  $goldfish := code→classSetProperty($goldfish, "name", "Goldie");
  $goldfish := code→classSetProperty($goldfish, "age", 1 ∥ "");
  $methodData := code→newMethodData($goldfish, "post");
  $methodData := code→methodDataSetParameter($methodData, "message", "Hello");
  code→classCallMethod($goldfish, $methodData);
  $incrAgeData := code→newMethodData($goldfish, "incrementAge");
  $incrAgeData := code→classCallMethod($goldfish, $incrAgeData);
  $newAge := code→methodDataGetReturnValueNumber($incrAgeData);
  $methodData := code→methodDataSetParameter($methodData, "message", "I just turned " ∥ $newAge);
  code→classCallMethod($goldfish, $methodData);
  $cat := code→newClass("cat");
  $cat := code→classSetProperty($cat, "name", "Kitty");
  $dog := code→newClass("dog");
  $dog := code→classSetProperty($dog, "name", "Fido");
  $catPost := code→newMethodData($cat, "post");
  $dogPost := code→newMethodData($dog, "post");
  $catPost := code→methodDataSetParameter($catPost, "message", "Hi " ∥ code→classGetPropertyString($dog, "name"));
  $dogPost := code→methodDataSetParameter($dogPost, "message", "Hi " ∥ code→classGetPropertyString($cat, "name"));
  code→classCallMethod($cat, $catPost);
  code→classCallMethod($dog, $dogPost);
  $catMeow := code→newMethodData($cat, "meow");
  $dogBark := code→newMethodData($dog, "bark");
  code→classCallMethod($cat, $catMeow);
  code→classCallMethod($dog, $dogBark);
}

action defineMethod(class: String, methodName: String, returnType: String) {
  $method := code→newObject;
  $method := code→objectAddString($method, "type", $returnType);
  $method := code→objectAddString($method, "name", $methodName);
  $method := code→objectAddJson($method, "parameters", code→newArray);
  $def := code→classlibGetDef($class);
  $methods := code→arrayAddJson($def→field("methods"), $method);
  $def := code→objectEditJson($def, "methods", $methods);
  code→classlibSetDef($def);
  meta private;
}

action defineParameter(class: String, method: String, parameterName: String, parameterType: String) {
  $param := code→newObject;
  $param := code→objectAddString($param, "type", $parameterType);
  $param := code→objectAddString($param, "name", $parameterName);
  $classes := code→classlibGetDefs();
  $class1 := code→newObject();
  $c := 0;
  while $c < $classes→count and $class1→keys→count = 0 do {
    $json := $classes→at($c);
    if $json→string("name")→equals($class) then {
      $class1 := $json;
    }
    else {
      $c := $c + 1;
    }
  }
  time→fail_if_not($class1→keys→count > 0);
  $methods := $class1→field("methods");
  $method1 := code→newObject();
  $m := 0;
  while $m < $methods→count and $method1→keys→count = 0 do {
    $json1 := $methods→at($m);
    if $json1→string("name")→equals($method) then {
      $method1 := $json1;
    }
    else {
      $m := $m + 1;
    }
  }
  $params := code→arrayAddJson($method1→field("parameters"), $param);
  $method1 := code→objectEditJson($method1, "parameters", $params);
  $methods := code→arrayEditJson($methods, $m, $method1);
  $class1 := code→objectEditJson($class1, "methods", $methods);
  code→classlibSetDefs(code→arrayEditJson($classes, $c, $class1));
  meta private;
}

action newClass(class: String) returns instance: Json_Object {
  $instance := code→newObject;
  $instance := code→objectAddString($instance, "type", $class);
  $inherits := code→newArray();
  $def := code→classlibGetDef($class);
  $defInherits := $def→string("inherits");
  while not $defInherits→is_empty do {
    $inherits := code→arrayAddString($inherits, $defInherits);
    $defInherits := code→classlibGetDef($defInherits)→string("inherits");
  }
  $instance := code→objectAddJson($instance, "inherits", $inherits);
  $props := $def→field("properties");
  for 0 ≤ i < $props→count do {
    $p := $props→at($i);
    $type := $p→string("type");
    $name := $p→string("name");
    if $type→equals("string") then {
      $instance := code→objectAddString($instance, $name, "");
    }
    else {
      if $type→equals("number") then {
        $instance := code→objectAddValue($instance, $name, 0 ∥ "");
      }
    }
  }
  meta private;
}

action classSetProperty(instance: Json_Object, property: String, value: String) returns updated: Json_Object {
  $updated := $instance;
  $class := $instance→string("type");
  $defs := code→classlibGetDefs();
  $def := code→newObject();
  $d := 0;
  while $d < $defs→count and $def→keys→count = 0 do {
    $json := $defs→at($d);
    if $json→string("name")→equals($class) then {
      $def := $json;
    }
    else {
      $d := $d + 1;
    }
  }
  time→fail_if_not($def→keys→count > 0);
  $props := $def→field("properties");
  for 0 ≤ i < $props→count do {
    $p := $props→at($i);
    $type := $p→string("type");
    $name := $p→string("name");
    if $name→equals($property) then {
      if $type→equals("string") then {
        $updated := code→objectEditString($instance, $property, $value);
      }
      else {
        $updated := code→objectEditValue($instance, $property, $value);
      }
    }
  }
  meta private;
}

action newMethodData(instance: Json_Object, method: String) returns dataObject: Json_Object {
  $class := $instance→string("type");
  $returnValue := code→newObject();
  $returnValue := code→objectAddString($returnValue, "type", "null");
  $returnValue := code→objectAddValue($returnValue, "value", "\"null\"");
  $dataObject := code→newObject;
  $dataObject := code→objectAddString($dataObject, "type", "methodData");
  $dataObject := code→objectAddJson($dataObject, "instance", $instance);
  $dataObject := code→objectAddString($dataObject, "method", $method);
  $dataObject := code→objectAddJson($dataObject, "returnValue", $returnValue);
  $dataObject := code→objectAddJson($dataObject, "parameters", code→newArray);
  $defs := code→classlibGetDefs();
  $def := code→newObject();
  $d := 0;
  while $d < $defs→count and $def→keys→count = 0 do {
    $json := $defs→at($d);
    if $json→string("name")→equals($class) then {
      $def := $json;
    }
    else {
      $d := $d + 1;
    }
  }
  time→fail_if_not($def→keys→count > 0);
  $methods := $def→field("methods");
  for 0 ≤ i < $methods→count do {
    $m := $methods→at($i);
    $mname := $m→string("name");
    if $mname→equals($method) then {
      $mtype := $m→string("type");
      if not $mtype→equals("null") then {
        $returnValue := code→objectEditString($returnValue, "type", $mtype);
        $dataObject := code→objectEditJson($dataObject, "returnValue", $returnValue);
      }
      $params := $m→field("parameters");
      for 0 ≤ j < $params→count do {
        $p := $params→at($j);
        $p := code→objectAddValue($p, "value", "\"null\"");
        $params := code→arrayEditJson($params, $j, $p);
      }
      $dataObject := code→objectEditJson($dataObject, "parameters", $params);
    }
  }
  meta private;
}

action methodDataSetParameter(methodData: Json_Object, parameter: String, value: String) returns updated: Json_Object {
  $updated := $methodData;
  $params := $methodData→field("parameters");
  for 0 ≤ i < $params→count do {
    $p := $params→at($i);
    $name := $p→string("name");
    if $name→equals($parameter) then {
      $type := $p→string("type");
      if $type→equals("string") then {
        $p := code→objectEditString($p, "value", $value);
        $params := code→arrayEditJson($params, $i, $p);
        $updated := code→objectEditJson($methodData, "parameters", $params);
      }
    }
  }
  meta private;
}

action testPetPost(pet: Json_Object, message: String) {
  (code→classGetPropertyString($pet, "name") ∥ " says:  " ∥ $message)→post_to_wall;
  meta private;
}

action classCallMethod(instance: Json_Object, methodData: Json_Object) returns updatedData: Json_Object {
  $updatedData := code→objectEditJson($methodData, "instance", $instance);
  $method := $methodData→string("method");
  $params := $methodData→field("parameters");
  $returnValue := $methodData→field("returnValue");
  if code→classIsType($instance, "pet") then {
    if $method→equals("post") then {
      code→testPetPost($instance, $params→at(0)→string("value"));
    }
    else {
      if $method→equals("incrementAge") then {
        $newAge := code→testPetIncrAge($instance);
        $returnValue := code→objectEditValue($returnValue, "value", $newAge ∥ "");
      }
    }
  }
  if code→classIsType($instance, "cat") then {
    if $method→equals("meow") then {
      code→testCatMeow($instance);
    }
  }
  if code→classIsType($instance, "dog") then {
    if $method→equals("bark") then {
      code→testDogBark($instance);
    }
  }
  $updatedData := code→objectEditJson($updatedData, "returnValue", $returnValue);
  meta private;
}

action classGetProperty(instance: Json_Object, property: String) returns value: Json_Object {
  $value := $instance→field($property);
  meta private;
}

action classGetPropertyString(instance: Json_Object, property: String) returns value: String {
  $value := code→classGetProperty($instance, $property) ∥ "";
  meta private;
}

action classGetPropertyNumber(instance: Json_Object, property: String) returns value: Number {
  $value := code→classGetProperty($instance, $property)→to_number;
  meta private;
}

action testPetIncrAge(pet: Json_Object) returns newAge: Number {
  $newAge := code→classGetPropertyNumber($pet, "age") + 1;
  code→classSetProperty($pet, "age", $newAge ∥ "");
  meta private;
}

action methodDataGetReturnValue(methodData: Json_Object) returns value: Json_Object {
  $value := $methodData→field("returnValue")→field("value");
  meta private;
}

action methodDataGetReturnValueString(methodData: Json_Object) returns value: String {
  $value := code→methodDataGetReturnValue($methodData) ∥ "";
  meta private;
}

action methodDataGetReturnValueNumber(methodData: Json_Object) returns value: Number {
  $value := code→methodDataGetReturnValue($methodData)→to_number;
  meta private;
}

action defineInheritedClass(name: String, inherits: String) {
  $classes := code→classlibGetDefs();
  $class := code→newObject;
  $class := code→objectAddString($class, "type", "class");
  $class := code→objectAddString($class, "name", $name);
  $class := code→objectAddString($class, "inherits", $inherits);
  $props := code→newArray;
  $methods := code→newArray;
  if not $inherits→is_empty then {
    $inherited := code→classlibGetDef($inherits);
    $props := $inherited→field("properties");
    $methods := $inherited→field("methods");
  }
  $class := code→objectAddJson($class, "properties", $props);
  $class := code→objectAddJson($class, "methods", $methods);
  code→classlibSetDefs(code→arrayAddJson($classes, $class));
  meta private;
}

action classlibGetDef(class: String) returns def: Json_Object {
  $classes := code→classlibGetDefs();
  $def := code→newObject();
  $c := 0;
  while $c < $classes→count and $def→keys→count = 0 do {
    $json := $classes→at($c);
    if $json→string("name")→equals($class) then {
      $def := $json;
    }
    else {
      $c := $c + 1;
    }
  }
  meta private;
}

action classlibSetDef(class: Json_Object) {
  $classes := code→classlibGetDefs();
  $name := $class→string("name");
  $c := 0;
  while $c < $classes→count do {
    if $classes→at($c)→string("name")→equals($name) then {
      code→classlibSetDefs(code→arrayEditJson(code→classlibGetDefs, $c, $class));
      $c := 99;
    }
    $c := $c + 1;
  }
  meta private;
}

action classIsType(instance: Json_Object, type: String) returns b: Boolean {
  $b := $instance→string("type")→equals($type);
  if not $b then {
    $inherits := $instance→field("inherits");
    $i := 0;
    while $i < $inherits→count and not $b do {
      $b := $inherits→at($i)→to_string→equals($type);
      $i := $i + 1;
    }
  }
  meta private;
}

action testCatMeow(cat: Json_Object) {
  code→testPetPost($cat, "Meow!");
  meta private;
}

action testDogBark(dog: Json_Object) {
  code→testPetPost($dog, "Woof!");
  meta private;
}

