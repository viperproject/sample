meta version "v2.2,nothing";
meta name "diversified demand";
meta icon "bolt";
meta color "#ff00008b";
// Calculate diversified demand for domestic electrical installation.
meta platform "current";

action demand(lighting: Number, heating\u0026power: Number, cooking: Number, cooking\u0026socket: Number, on_demand_water_heating: Number, thermostatic_water_heating: Number, floor_heating: Number, storage_heaters: Number, largest_other_standard_circuit: Number, other_standard_circuits: Number) {
  $demand := $lighting * 0.66;
  $temp := 0;
  if $heating\u0026power > 10 then {
    $temp := 10 + (($heating\u0026power - 10) / 2);
  }
  else {
    $temp := $heating\u0026power;
  }
  $demand := $demand + $temp;
  if $cooking > 10 then {
    $temp := 10 + (($cooking - 10) * 0.3);
  }
  else {
    $temp := $cooking;
  }
  $demand := $demand + $temp;
  if $cooking\u0026socket > 10 then {
    $temp := 10 + (($cooking - 10) * 0.3);
  }
  else {
    $temp := $cooking\u0026socket;
  }
  if $cooking\u0026socket > 0 then {
    $temp := $temp + 5;
  }
  $demand := $demand + $temp;
  $temp := $thermostatic_water_heating + $on_demand_water_heating + $floor_heating + $storage_heaters;
  $demand := $demand + $temp;
  $temp := $largest_other_standard_circuit + 0.4 * $other_standard_circuits;
  $demand := $demand + $temp;
  $result := "Max Demand taking diversity into account is: " ∥ $demand;
  wall→create_text_box($result, 19)→post_to_wall;
}
table circuits {
  // circuits in an installation
  type = "Table";
  fields {
    lighting: Number_field
    heating\u0026power: Number_field
    cooking: Number_field
    cooking\u0026socket: Number_field
    on_demand_water_heating: Number_field
    thermostatic_water_heating: Number_field
    floor_heating: Number_field
    storage_heaters: Number_field
    largest_other_standard_circuit: Number_field
    other_standard_circuits: Number_field
  }
}

action store(lighting: Number, heating\u0026power: Number, cooking: Number, cooking\u0026socket: Number, on_demand_water_heating: Number, thermostatic_water_heating: Number, floor_heating: Number, storage_heaters: Number, largest_other_standard_circuit: Number, other_standard_circuits: Number) {
  $g := records→circuits_table→add_row;
  $g→lighting→set($lighting);
  $g→heating\u0026power→set($heating\u0026power);
  $g→cooking→set($cooking);
  $g→cooking\u0026socket→set($cooking\u0026socket);
  $g→on_demand_water_heating→set($on_demand_water_heating);
  $g→thermostatic_water_heating→set($thermostatic_water_heating);
  $g→floor_heating→set($floor_heating);
  $g→storage_heaters→set($storage_heaters);
  $g→largest_other_standard_circuit→set($largest_other_standard_circuit);
  $g→other_standard_circuits→set($other_standard_circuits);
  meta private;
}

