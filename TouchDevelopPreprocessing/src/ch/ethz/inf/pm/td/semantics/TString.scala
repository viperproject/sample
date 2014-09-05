
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of String
 *
 * A piece of text
 *
 * Abstracted by its length
 *
 * @author Lucas Brutschy
 */

object TString extends AAny {

  /** Returns the number of characters */
  //TODO lazy val field_count = new TouchField("count",TNumber.typeName)

  val typeName = TypeName("String")

  override def possibleFields = super.possibleFields ++ List(/*TODO field_count*/)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Gets the character at a specified index */
    case "at" =>
      val List(index) = parameters // Number
      Dummy[S](this0, method)
      Top[S](TString)

    /** Gets the character at a specified index */
    case "at index" =>
      val List(index) = parameters // Number
      Dummy[S](this0, method)
      Top[S](TString)

    /** Compares two pieces of text */
    case "compare" =>
      val List(other) = parameters // String
      Dummy[S](this0, method)
      Top[S](TNumber)

    /** Gets the length of the string */
    case "count" =>
      Dummy[S](this0, method)
      Return[S](0 ndTo PositiveInfinity)

    /** Concatenates two pieces of text */
    case "concat" =>
      Dummy[S](this0, method)
      Top[S](TString)

    /** Returns a value indicating if the second string is contained */
    case "contains" =>
      Dummy[S](this0, method)
      val List(value) = parameters // String
      Top[S](TBoolean)

    /** Concatenates two pieces of text */
    case "copy" =>
      Clone[S](this0)

    /** Stores text in the clipboard */
    case "copy to clipboard" =>
      Dummy[S](this0, method)
      Skip

    /** Determines whether the ending matches the specified string */
    case "ends with" =>
      Dummy[S](this0, method)
      val List(value) = parameters // String
      Top[S](TBoolean)

    /** Checks if two strings are the same */
    case "equals" =>
      val List(other) = parameters
      val ret = Return[S](this0 equal other)
      ret

    /** Returns the index of the first occurence if found starting at a given position */
    case "index of" =>
      Dummy[S](this0, method)
      val List(value, start) = parameters // String,Number
      Top[S](TNumber)

    /** Inserts a string at a given position */
    case "insert" =>
      Dummy[S](this0, method)
      val List(start, value) = parameters // Number,String
      Top[S](TString)

    /** Indicates if the string is empty */
    case "is empty" =>
      Dummy[S](this0, method)
      Top[S](TBoolean)

    /** Indicates if the string matches a regular expression */
    case "is match regex" =>
      Dummy[S](this0, method)
      val List(pattern) = parameters // String
      Top[S](TBoolean)

    /** Returns the index of the last occurence if found starting at a given position */
    case "last index of" =>
      Dummy[S](this0, method)
      val List(value, start) = parameters // String,Number
      Top[S](TNumber)

    /** Gets the groups from the matching the regex expression (pattern) */
    case "match" =>
      val List(pattern) = parameters // String
      Top[S](TString_Collection)

    /** Gets the strings matching the regex expression (pattern) */
    case "matches" =>
      Dummy[S](this0, method)
      val List(pattern) = parameters // String
      Top[S](TString_Collection)

    /** Returns the string with characters removed starting at a given index */
    case "remove" =>
      Dummy[S](this0, method)
      val List(start) = parameters // Number
      Top[S](TString)

    /** Returns a given string with a replacement */
    case "replace" =>
      Dummy[S](this0, method)
      val List(old, newS) = parameters // String,String
      Top[S](TString)

    /** Replace every match of the regex according to the replacement string */
    case "replace regex" =>
      Dummy[S](this0, method)
      val List(pattern, replace) = parameters // String,String
      Top[S](TString)

    /** Shares the string (email, sms, facebook, social or '' to pick from a list) */
    case "share" =>
      Dummy[S](this0, method)
      val List(network) = parameters // String
      Skip

    /** Returns a string collection that contains the substrings in this string that are delimited by elements of a specified string. */
    case "split" =>
      Dummy[S](this0, method)
      val List(separator) = parameters // String
    // No matter what the arguments are, the resulting set has at least one element!
    var curState = state
      curState = Top[S](TString_Collection)(curState, pp)
      val obj = curState.expr
      curState = Assume(CollectionSize[S](obj) >= 1)(curState, pp)
      Return[S](obj)(curState, pp)

    /** Determines whether the beginning matches the specified string */
    case "starts with" =>
      Dummy[S](this0, method)
      val List(value) = parameters // String
      Top[S](TBoolean)

    /** Returns a substring given a start index and a length */
    case "substring" =>
      Dummy[S](this0, method)
      val List(start, length) = parameters // Number,Number
      Top[S](TString)

    /** Parses the string as a boolean */
    case "to boolean" =>
      Dummy[S](this0, method)
      TopWithInvalid[S](TBoolean, "conversion to boolean may fail")

    /** Parses the string as a color. */
    case "to color" =>
      Dummy[S](this0, method)
      TopWithInvalid[S](TColor, "conversion to color may fail")

    /** Parses the string as a date and time. */
    case "to datetime" =>
      Dummy[S](this0, method)
      TopWithInvalid[S](TDateTime, "conversion to datetime may fail")

    /** Parses the string as a geo coordinate. */
    case "to location" =>
      Dummy[S](this0, method)
      TopWithInvalid[S](TLocation, "conversion to location may fail")

    /** Returns a copy of this string converted to lowercase, using the casing rules of the current culture. */
    case "to lower case" =>
      Dummy[S](this0, method)
      Top[S](TString)

    /** Parses the string as a number */
    case "to number" =>
      Dummy[S](this0, method)
      Top[S](TNumber)
    // FIXME: UNSOUNDNESS: TopWithInvalid[S](TNumber)

    /** Parses the string as a time (12:30:12) and returns the number of seconds. */
    case "to time" =>
      Dummy[S](this0, method)
      TopWithInvalid[S](TNumber, "conversion to time may fail")

    /** Converts a single character string into its unicode number */
    case "to unicode" =>
      Dummy[S](this0, method)
      Top[S](TNumber)

    /** Returns a copy of this string converted to uppercase, using the casing rules of the current culture. */
    case "to upper case" =>
      Dummy[S](this0, method)
      Top[S](TString)

    /** Removes all leading and trailing occurrences of a set of characters specified in a string from the current string. */
    case "trim" =>
      Dummy[S](this0, method)
      val List(chars) = parameters // String
      Top[S](TString)

    /** Removes all trailing occurrences of a set of characters specified in a string from the current string. */
    case "trim end" =>
      Dummy[S](this0, method)
      val List(chars) = parameters // String
      Top[S](TString)

    /** Removes all leading occurrences of a set of characters specified in a string from the current string. */
    case "trim start" =>
      Dummy[S](this0, method)
      val List(chars) = parameters // String
      Top[S](TString)

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
