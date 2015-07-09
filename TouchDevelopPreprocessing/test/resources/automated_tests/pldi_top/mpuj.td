meta version "v2.2,js,ctx,refs,localcloud,unicodemodel,allasync";
meta name "My Online Meetings";
meta icon "phone";
meta color "#ff008000";
meta rootId "JDCK69GjVj4lBdCYtdN4CTcN";
meta platform "current";
meta parentIds "";
#MyConfCallsv12e1
action My_ConfCalls_v1\u002e1() {
  // This program parses your calender for conference calls and dials them on request including ConferenceIDs
  data→release := "1.1a";
  data→nextdays := 0;
  data→meetings := 0;
  $advanced := true;
  // Init dial conference hub
  if data→dialin→is_empty then {
    code→setDialin;
  }
  $sDialin := data→dialin;
  $sDialin := data→dialin;
  // Init LeaderName
  if data→sLeaderName→is_empty then {
    code→setLeader;
  }
  $sLeaderName := data→sLeaderName;
  // Init PIN
  if data→sPIN→is_empty then {
    code→setPIN;
  }
  $sPIN := data→sPIN;
  $s := time→today→add_days(0);
  $e := time→tomorrow→add_days(data→nextdays);
  $sID, $sLeader := code→getConfCalls($s, $e);
  code→setTile;
  //:: ExpectedOutput(assert.failed)
  if $sID→equals("") then {
    skip;
  }
  else {
    $r := code→getDialString($sDialin, $sID, $sLeader, $sPIN, $advanced);
    if $r→contains("@") then {
      code→setDialin;
      if $advanced then {
        code→setLeader;
        code→setPIN;
      }
      code→My_ConfCalls_v1\u002e1;
    }
    else {
      code→showDTMFControls;
      phone→dial_phone_number($r);
    }
  }
  "\n\n" →post_to_wall;
}
#getDialString
action getDialString(PhoneNumber: String, ConferenceID: String, Organizer: String, PIN: String, Advanced: Boolean) returns(DialString: String) {
  $p := $PhoneNumber;
  $c := $ConferenceID;
  $w := " pp";
  $h := "#";
  $DialString := "";
  if $Advanced then {
    //:: ExpectedOutput(assert.failed)
    $yadl := data→sLeaderName→equals($Organizer);
    if $yadl then {
      $DialString := $p ∥ $w ∥ $c ∥ $h ∥ $w ∥ "*" ∥ data→sPIN;
    }
    else {
      $DialString := $p ∥ $w ∥ $c ∥ $h;
    }
  }
  else {
    $DialString := $p ∥ $w ∥ $c ∥ $h;
  }
  meta private;
}
#getConfCalls
action getConfCalls(start: DateTime, end: DateTime) returns(ID: String, Leader: String) {
  $ID := "";
  $validConfID := false;
  $Leader := "";
  $s := "";
  $s1 := "";
  $s2 := "";
  $s3 := "";
  $s4 := "";
  $p1 := "";
  $subject := "-";
  $location := "-";
  $t1 := "Today\'s Virtual Meetings for  " ∥ data→sLeaderName;
  $t2 := "Select session to dial conf hub " ∥ data→dialin;
  $appointments := social→search_appointments($start, $end);
  foreach a1 in $appointments
  do {
    if $a1→subject→is_invalid then {
      $subject := "subject not specified";
    }
    else {
      $subject := $a1→subject;
    }
    if $a1→location→is_invalid then {
      $location := "location not specified";
    }
    else {
      $location := $a1→location;
    }
    if($a1→details→is_invalid) then {
      // No meeting details
    }
    else {
      $s, $validConfID := code→getConfID_Regex($a1);
      if `not` $validConfID then {
        // No ConferenceID found
      }
      else {
        data→meetings := data→meetings + 1;
        $h0 := code→getShortDate($a1→start_time);
        $h1 := code→getTime($a1→start_time);
        $h2 := code→getTime($a1→end_time);
        $h3 := $h1 ∥ " - " ∥ $h2;
        $s1 := $s1 ∥ $h3 ∥ "\n";
        $s1 := $s1 ∥ code→getString($subject) ∥ "\n";
        $s1 := $s1 ∥ code→getString($location) ∥ "\n";
        $s1 := $s1 ∥ code→getString($a1→organizer ∥ "") ∥ ", ";
        $s1 := $s1 ∥ "id: " ∥ $s ∥ "\n";
        $s1 := $s1 ∥ $a1→attendees→count + 1 ∥ " attendees. My status = " ∥ code→getStatus($a1);
        $s1 := $s1 ∥ ";";
        $s2 := $s2 ∥ $s ∥ ";";
        $s3 := $s3 ∥ $a1→organizer ∥ ";";
        $s4 := $s4 ∥ $p1 ∥ ";";
      }
    }
  }
  $s1 := $s1 ∥ "Press <here> to change your settings";
  skip;
  $s2 := $s2 ∥ "@";
  $s3 := $s3 ∥ "end";
  $s4 := $s4 ∥ "end";
  $MeetingCollection := $s1→split(";");
  $ConfIDCollection := $s2→split(";");
  $LeaderCollection := $s3→split(";");
  $phoneCollection := $s4→split(";");
  if data→meetings ≤ 0 then {
    $z := code→getShortDate(time→today) ∥ ": No Virtual Meetings scheduled for Today.";
    $z→post_to_wall;
  }
  else {
    $index := wall→pick_string($t1, $t2, $MeetingCollection);
    $ID := $ConfIDCollection→at($index);
    $Leader := $LeaderCollection→at($index);
  }
  meta private;
}
#getConfID
action getConfID(app: Appointment) returns(r: String) {
  $r := - 1 ∥ "";
  $SearchString := "Conference ID: ";
  $Offset := $SearchString→count;
  $ConfIDLength := 10;
  if $app→details→is_empty then {
    // no ConferenceID to parse
  }
  else {
    $x := $app→details→index_of($SearchString, 0);
    if $x < 0 then {
      // no ConferenceID found
    }
    else {
      $x := $x + $Offset;
      $s := $app→details→substring($x, $ConfIDLength);
      $t := $s→to_number;
      $r := $t→to_string;
    }
  }
  meta private;
}
#dialin
var dialin : String {
  // Phone number conference hub
}
#setDialin
action setDialin() {
  wall→clear;
  $s := "Enter Dialin Number to Conference Hub";
  $t1 := "Change Dial Number?";
  if data→dialin→is_empty then {
    data→dialin := wall→ask_string($s);
  }
  else { if wall→ask_boolean($t1, data→dialin) then {
    data→dialin := wall→ask_string($s);
  }
  else { } }
  wall→clear;
  meta private;
}
#meetings
var meetings : Number {
  // Todays Count of Confrence Calls
}
#setTile
action setTile() {
  $sToday := code→getShortDate(time→today);
  data→My_ConfCalls_v1\u002e1_tile→set_counter(data→meetings);
  data→My_ConfCalls_v1\u002e1_tile→set_content($sToday);
  data→My_ConfCalls_v1\u002e1_tile→set_background(colors→accent);
  skip;
  meta private;
}
#nextdays
var nextdays : Number {
  // number of days to parse your calender for confcalls
}
#getTime
action getTime(dt1: DateTime) returns(s1: String) {
  $sHour := $dt1→hour→to_string;
  $sMin := $dt1→minute→to_string;
  if $sHour→count = 1 then {
    $sHour := "0" ∥ $sHour;
  }
  if $sMin→count = 1 then {
    $sMin := "0" ∥ $sMin;
  }
  $s1 := $sHour ∥ ":" ∥ $sMin;
  meta private;
}
#getString
action getString(s1: String) returns(s2: String) {
  if $s1→is_empty then {
    $s1 := " ";
  }
  if $s1→contains(";") then {
    $s1→replace(";", ":");
  }
  $s2 := $s1;
  meta private;
}
#getShortDate
action getShortDate(dt1: DateTime) returns(s: String) {
  $csWeekDays := "Sun,Mon,Tue,Wed,Thu,Fri,Sat" →split(",");
  $sDate := $dt1→day→to_string;
  $x := $dt1→week_day;
  $sWeekDay := $csWeekDays→at($x);
  $s := $sWeekDay ∥ " " ∥ $sDate;
  meta private;
}
#setLeader
action setLeader() {
  wall→clear;
  $s := "Enter your (organizer) Name";
  $t1 := "Change your Name?";
  if data→sLeaderName→is_empty then {
    data→sLeaderName := wall→ask_string($s);
  }
  else { if wall→ask_boolean($t1, data→sLeaderName) then {
    data→sLeaderName := wall→ask_string($s);
  }
  else { } }
  wall→clear;
  meta private;
}
#sLeaderName
var sLeaderName : String {
}
#setPIN
action setPIN() {
  wall→clear;
  $s := "Enter your PIN";
  $t1 := "Change your PIN?";
  if data→sPIN→is_empty then {
    data→sPIN := wall→ask_string($s);
  }
  else { if wall→ask_boolean($t1, data→sPIN) then {
    data→sPIN := wall→ask_string($s);
  }
  else { } }
  wall→clear;
  meta private;
}
#sPIN
var sPIN : String {
}
#resetSettings
action resetSettings() {
  data→dialin := "";
  data→sLeaderName := "";
  data→sPIN := "";
  meta private;
}
#showDTMFControls
action showDTMFControls() {
  $s0 := "\nIN CONFERENCE DTMF CONTROLS\n\n*6 Mute or unmute your microphone\n*4 Toggle audience mute\n*7 Lock or unlock the conference\n*9 Enable announcements for participants\n*3 Privately play active participants names\n*1 Play a descripion of this text\n*8 Admit lobby participants to the call";
  $s1 := "\n\n\n\n";
  $s2 := "Have a good Virtual Meeting!\n\n\n";
  $s3 := "You are currently using version " ∥ data→release ∥ "\n\n";
  $s4 := "\nTo stay tuned with updated versions? \nFollow me on Twitter @peterheldens\n";
  $s := $s0 ∥ $s1 ∥ $s2 ∥ $s3 ∥ $s4;
  wall→clear;
  $s→post_to_wall;
  meta private;
}
#getStatus
action getStatus(a1: Appointment) returns(s1: String) {
  $s1 := $a1→status;
  if $s1→equals("busy") then {
    $s1 := "accepted";
  }
  meta private;
}
#getConfIDRegex
action getConfID_Regex(a1: Appointment) returns(confID: String, validID: Boolean) {
  $validID := false;
  $strAppDetails := $a1→details;
  $searchConfID := "Conference ID:";
  $regexpConfID := $searchConfID ∥ "\\s*[\\d]\\d*";
  $cConfID := $strAppDetails→matches($regexpConfID);
  $sConfID := $cConfID→join("");
  $confID := $sConfID→replace_regex($searchConfID, "");
  $confID := $confID→trim_start(" ");
  if $confID→count < 1 then {
    $validID := false;
  }
  else {
    $validID := true;
  }
  meta private;
}
#release
var release : String {
}
#MyConfCallsv12e1tile
var My_ConfCalls_v1\u002e1_tile : Tile {
  readonly = true;
  transient = true;
}