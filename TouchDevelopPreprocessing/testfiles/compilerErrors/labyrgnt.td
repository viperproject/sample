meta version "v2.2,nothing";
meta name "SIIIdb";
meta icon "shield";
meta color "#ff00008b";
// Security database
meta platform "current";
table officertbl {
  type = "Table";
  fields {
    OfficerID: Number_field
    officerFirst: String_field
    officerLast: Number_field
  }
}
table shiftTbl {
  type = "Table";
  keys {
    officertbllink: officertbl
  }
  fields {
    shiftStart: DateTime_field
    shiftEnd: DateTime_field
    officer: officertbl_field
  }
}

