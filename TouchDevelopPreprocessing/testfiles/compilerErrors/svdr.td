meta version "v2.2,nothing";
meta name "Toddlers Word Counter";
meta icon "book";
// Count the words that your toddler knows how to say. A good record for the Dr. or school.
meta platform "current";

action main() {
  "TouchDevelop is cool!" →post_to_wall;
}

event phone_landscape_left() {
  skip;
  meta private;
}

event phone_landscape_right() {
  skip;
  meta private;
}
table Two_Words {
  // Two word together that your toddlers says
  type = "Table";
  fields {
    x: Number_field
    x0: Number_field
    x1: Number_field
  }
}
table Single_Word {
  // One word that your toddler says
  type = "Table";
  fields {
    x: Number_field
    x0: Number_field
    x1: Number_field
  }
}
table Three_Words {
  // Three words put together.
  type = "Table";
  fields {
    x: Number_field
    x0: Number_field
    x1: Number_field
  }
}

