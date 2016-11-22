action main() {
   // Using a table
   //$Thing := records→Thing_table→add_row;
   //$Thing2 := records→Thing_table→row_at(0);
   //$ref := $Thing2→f2;
   //$ref := colors→red;
   //$Thing4 := records→Thing4_index→singleton;
   //$ref2 := $Thing2→f2→\u25c8ref;
   //$ref2→\u25c8set(colors→red);
   //$Thing2→post_to_wall;
}

table Thing {
  type = "Table";
  cloudenabled = true;
  persistent = true;
  fields {
     f : Boolean
     f2 : Color
     f3 : DateTime
     f4 : Json_Object
     f5 : Location
     f6 : Link
     f7 : Number
     f8 : OAuth_Response
     f9 : String
     f10 : User
  }
}

table Thing2 {
  type = "Index";
  cloudenabled = true;
  persistent = true;
  keys {
     k : Boolean
     k2 : Color
     k3 : DateTime
     k4 : Location
     k5 : Number
     k6 : String
     k7 : User
  }
  fields {
     f : String
  }
}

table Thing3 {
  type = "Table";
  cloudenabled = true;
  persistent = true;
  keys {
     l : Thing
  }
  fields {
     f : String
  }
}

table Thing4 {
  type = "Index";
  cloudenabled = true;
  persistent = true;
  fields {
     f : String
  }
}