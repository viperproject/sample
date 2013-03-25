meta version "v2.2,nothing";
meta name "coding guidelines";
meta icon "Award";
meta color "#ff007FFF";
// This is a script that showcases the preferred coding and naming conventions for libraries.
meta isLibrary "yes";
meta platform "current";

action example() {
  // ## conventions
  // ### actions
  // Lower case with spaces as necessary.
  // * good: `action do stuff`
  // * bad: `action doStuff`, don't use camel notation
  // * bad: `action DoStuff', don't use capitalized notation
  // * bad: `action do_stuff`, don't use underscore, use space instead
  // ### action parameters
  // Use meaningful names rather than initials.
  // * good: 'action do stuff(height : Number)'
  // * bad: 'action do stuff(h : Number)'
  // ## documentation
  // ### actions
  // * The description of an action is added by writing a comment on the first line. The description is displayed in various places to the user.
  // ### libraries
  // * actions named `example...` will be considered documentation in libraries.
  // * comments support a Wiki syntax (subset of Markdown). See the source of this action.
}

