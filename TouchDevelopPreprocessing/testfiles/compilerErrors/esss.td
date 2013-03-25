meta version "v2.2,nothing";
meta name "tiltDial";
meta icon "command";
meta color "#ffff2800";
// This is just a demonstration of a menu system based on a menu tree and a user interface which relies on tilting the phone. You can select one of a large number of contacts, using the fact that they are in alphabetical order. Tilt right to select, left to go back. You MUST set the privacy to allow non-anonymised contacts for this to work. Otherwise you will just see Some Body as a choice.
meta platform "current,contacts,phone";

action main() {
  code→init;
  if records→contactNumbers_table→count = 0 then {
    code→getContactNumbers;
  }
  $menuItems := collections→create_string_collection;
  $index := collections→create_number_collection;
  $nToDisplay := collections→create_number_collection;
  foreach e in records→contactNumbers_table where true do {
    $menuItems→add($e→name→get);
  }
  $startNode := code→buildMenuTree($menuItems, $index, $nToDisplay, data→maxMenuItems);
  $selection := code→tiltSelectFromMenuTree($menuItems, $index, $nToDisplay, $startNode, "Turn over to reload");
  if $selection = - 10 then {
    code→getContactNumbers;
    $selection := code→tiltSelectFromMenuTree($menuItems, $index, $nToDisplay, $startNode, "Turn over to reload");
  }
  $t := 0;
  foreach e2 in records→contactNumbers_table where true do {
    if $t = $selection then {
      wall→clear;
      ("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nWill call " ∥ $e2→name→get ∥ " on " ∥ $e2→number→get→address)→post_to_wall;
      phone→dial_phone_number($e2→number→get→address);
    }
    $t := $t + 1;
    // should have used an index instead of a table
  }
}

action buildMenuTree(menuItems: String_Collection, index: Number_Collection, nToDisplay: Number_Collection, maxItems: Number) returns startNode: Number {
  while $index→count > 0 do {
    $index→remove_at(0);
  }
  while $nToDisplay→count > 0 do {
    $nToDisplay→remove_at(0);
  }
  $newIndex := collections→create_number_collection;
  $newNToDisplay := collections→create_number_collection;
  $levelStart := 0;
  $nItemsForLevel := $menuItems→count;
  for 0 ≤ i < $nItemsForLevel do {
    $index→add( - 1);
    $nToDisplay→add(0);
  }
  while $nItemsForLevel > 1 do {
    $oldNItems := $nItemsForLevel;
    $oldLevelStart := $levelStart;
    $levelStart := $levelStart + $oldNItems;
    $nItemsForLevel := math→floor(($oldNItems - 1) / $maxItems) + 1;
    for 0 ≤ j < $nItemsForLevel do {
      $index→add($oldLevelStart + $j * $maxItems);
      if $j < $nItemsForLevel - 1 then {
        $nToDisplay→add($maxItems);
      }
      else {
        $nToDisplay→add(math→mod($oldNItems - 1, $maxItems) + 1);
      }
    }
  }
  $startNode := $levelStart;
  meta private;
}

action tiltSelectString(menuItems: String_Collection, finalLevel: Boolean, startChoice: Number, prompt: String) returns selection: Number {
  wall→clear;
  $nChoice := $menuItems→count;
  for 0 ≤ i < $nChoice do {
    data→menuSprites→at($i)→set_text($menuItems→at($i));
    data→menuSprites→at($i)→set_color(data→unChosenColor);
    data→menuSprites→at($i)→show;
  }
  for 0 ≤ j < data→maxMenuItems - $nChoice do {
    data→menuSprites→at($nChoice + $j)→hide;
  }
  data→menuSprites→at(data→maxMenuItems)→set_text($prompt);
  data→menuSprites→at(data→maxMenuItems)→show;
  data→menuSprites→at(data→maxMenuItems + 1)→set_text("*****");
  data→menuSprites→at(data→maxMenuItems + 1)→set_opacity(0.5);
  data→menuSprites→at(data→maxMenuItems + 1)→show;
  if data→debugging then {
    data→menuSprites→at(data→maxMenuItems + 2)→show;
  }
  $currentChoice := $startChoice;
  data→menuSprites→at($startChoice)→set_color(data→chosenColor);
  $wasTiltedLeft := false;
  $wasTiltedRight := false;
  if senses→orientation→x < data→confirmTiltAngle * - 1 then {
    $wasTiltedLeft := true;
  }
  else {
    if senses→orientation→x > data→confirmTiltAngle then {
      $wasTiltedRight := true;
    }
  }
  $selection := - 2;
  data→menuBoard→post_to_wall;
  $yDiffTot := 0;
  while $selection = - 2 do {
    if data→debugging then {
      data→menuSprites→at(data→maxMenuItems + 2)→set_text(" x=" ∥ senses→orientation→x ∥ "\ny=" ∥ senses→orientation→y ∥ "\nDiffTot=" ∥ $yDiffTot);
    }
    $cursorPos := (data→height + data→gap) * ($currentChoice + 1) + $yDiffTot / data→yDiffThreshold * (data→height + data→gap);
    if $cursorPos < data→height + data→gap then {
      $cursorPos := data→height + data→gap;
    }
    else {
      if $cursorPos > (data→height + data→gap) * $nChoice then {
        $cursorPos := (data→height + data→gap) * $nChoice;
      }
    }
    data→menuSprites→at(data→maxMenuItems + 1)→set_pos(240, $cursorPos);
    data→menuBoard→evolve;
    data→menuBoard→update_on_wall;
    if senses→orientation→x < data→confirmTiltAngle * - 1 and not $wasTiltedLeft then {
      $selection := - 1;
    }
    else {
      if senses→orientation→x > data→confirmTiltAngle and not $wasTiltedRight then {
        $selection := $currentChoice;
      }
      else {
        if senses→orientation→x > data→confirmTiltAngle * - 1 then {
          $wasTiltedLeft := false;
        }
        if senses→orientation→x < data→confirmTiltAngle then {
          $wasTiltedRight := false;
        }
        $yDiff := senses→orientation→y - data→baseSelectAngle;
        $signYDiff := math→sign($yDiff);
        $yDiff := math→abs($yDiff) - data→selectTiltAngle;
        if $yDiff < 0 then {
          $yDiff := 0;
        }
        $yDiff := $yDiff * $yDiff * $signYDiff;
        // change of direction - pause a moment?
        if math→sign($yDiff) ≠ math→sign($yDiffTot) then {
          $yDiffTot := 0;
        }
        $yDiffTot := $yDiffTot + $yDiff;
        $delayFrac := 1 - (math→abs($yDiff) - data→selectTiltAngle) / data→tiltRange;
        if $delayFrac < 0 then {
          $delayFrac := 0;
        }
        else {
          if $delayFrac > 1 then {
            $delayFrac := 1;
          }
        }
        if $yDiffTot > data→yDiffThreshold and $currentChoice < $nChoice - 1 then {
          $yDiffTot := 0;
          data→menuSprites→at($currentChoice)→set_color(data→unChosenColor);
          $currentChoice := $currentChoice + 1;
          data→menuSprites→at($currentChoice)→set_color(data→chosenColor);
        }
        else {
          if $yDiffTot < data→yDiffThreshold * - 1 and $currentChoice > 0 then {
            $yDiffTot := 0;
            data→menuSprites→at($currentChoice)→set_color(data→unChosenColor);
            $currentChoice := $currentChoice - 1;
            data→menuSprites→at($currentChoice)→set_color(data→chosenColor);
          }
        }
        time→sleep(.02);
        if senses→orientation→y < - 120 or senses→orientation→y > 120 then {
          $selection := - 10;
        }
      }
    }
  }
  meta private;
}

action init() {
  data→debugging := false;
  data→maxMenuItems := 10;
  data→height := 40;
  data→gap := 10;
  data→fontsize := 40;
  data→width := 200;
  data→confirmTiltAngle := 10;
  data→selectTiltAngle := 10;
  data→baseSelectAngle := 30;
  data→menuDelay := 1;
  data→tiltRange := 40;
  data→yDiffThreshold := 1000;
  data→unChosenColor := colors→orange;
  data→chosenColor := colors→red;
  data→finalUnchosenColor := colors→green;
  data→finalChosenColor := colors→red;
  data→menuBoard := media→create_portrait_board(480, 800);
  data→menuSprites := data→menuBoard→create_sprite_set;
  for 0 ≤ i < data→maxMenuItems + 3 do {
    $sprite := data→menuBoard→create_text(data→width, data→height, data→fontsize, "test");
    $sprite→set_pos(100, (data→height + data→gap) * ($i + 1));
    $sprite→hide;
    data→menuSprites→add($sprite);
  }
  meta private;
}
var maxMenuItems : Number {
}
var unChosenColor : Color {
}
var chosenColor : Color {
}
var finalUnchosenColor : Color {
}
var finalChosenColor : Color {
}
var height : Number {
}
var gap : Number {
}
var fontsize : Number {
}
var width : Number {
}
var menuBoard : Board {
}
var menuSprites : Sprite_Set {
}
var confirmTiltAngle : Number {
}
var menuDelay : Number {
}

action tiltSelectFromMenuTree(menuItems: String_Collection, index: Number_Collection, nToDisplay: Number_Collection, startNode: Number, prompt: String) returns selection: Number {
  $thisMenu := collections→create_string_collection;
  $indexStack := collections→create_number_collection;
  $stackPos := 0;
  $indexStack→add($startNode);
  $selection := - 2;
  $s := $startNode;
  $startLab := "";
  $endLab := "";
  while $selection = - 2 do {
    while $thisMenu→count > 0 do {
      $thisMenu→remove_at(0);
    }
    for 0 ≤ i < $nToDisplay→at($s) do {
      $labIndex := $index→at($s) + $i;
      while not $index→at($labIndex) = - 1 do {
        $labIndex := $index→at($labIndex);
      }
      $startLab := $menuItems→at($labIndex);
      $labIndex := $index→at($s) + $i;
      while not $index→at($labIndex) = - 1 do {
        $labIndex := $index→at($labIndex) + $nToDisplay→at($labIndex) - 1;
      }
      $endLab := $menuItems→at($labIndex);
      if not $endLab→equals($startLab) then {
        $startLab := $startLab ∥ " - " ∥ $endLab;
      }
      $thisMenu→add($startLab);
    }
    $sss := code→tiltSelectString($thisMenu, false, 0, $prompt);
    if $sss < 0 then {
      if $sss = - 1 then {
        if $stackPos > 0 then {
          $indexStack→remove_at($stackPos);
          $stackPos := $stackPos - 1;
          $s := $indexStack→at($stackPos);
        }
        else {
          $s := $startNode;
        }
      }
      else {
        if $sss = - 10 then {
          $selection := - 10;
        }
      }
    }
    else {
      $stackPos := $stackPos + 1;
      $s := $index→at($s) + $sss;
      $indexStack→add($s);
      if $index→at($s) = - 1 then {
        $selection := $s;
      }
    }
  }
  meta private;
}
var selectTiltAngle : Number {
}
var baseSelectAngle : Number {
}
var debugging : Boolean {
}
var tiltRange : Number {
}
var yDiffThreshold : Number {
}
table contactNumbers {
  type = "Table";
  fields {
    name: String_field
    number: Link_field
  }
}

action getContactNumbers() {
  wall→clear;
  "Please wait while numbers are collected from contacts (this only has to be done once)" →post_to_wall;
  $contacts := social→search_contacts("");
  foreach e2 in records→contactNumbers_table where not $e2→name→get→is_invalid do {
    $e2→delete_row;
  }
  foreach e in $contacts where true do {
    if not $e→home_phone→is_invalid then {
      $r := records→contactNumbers_table→add_row;
      $r→name→set("H " ∥ $e→name);
      $r→number→set($e→home_phone);
    }
    if not $e→work_phone→is_invalid then {
      $r1 := records→contactNumbers_table→add_row;
      $r1→name→set("W " ∥ $e→name);
      $r1→number→set($e→work_phone);
    }
    if not $e→mobile_phone→is_invalid then {
      $r2 := records→contactNumbers_table→add_row;
      $r2→name→set("M " ∥ $e→name);
      $r2→number→set($e→mobile_phone);
    }
  }
  meta private;
}

