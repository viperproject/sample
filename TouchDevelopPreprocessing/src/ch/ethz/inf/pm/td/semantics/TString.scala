
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.sample.Reporter

/**
 * Specifies the abstract semantics of String
 *
 * A piece of text
 *
 * Abstracted by its length
 *
 * @author Lucas Brutschy
 */ 

object TString {

  /** Returns the number of characters */
  //TODO val field_count = new TouchField("count",TNumber.typ)

  val typName = "String"
  val typ = new TouchType(typName,isSingleton = false, fields = List(/*TODO field_count*/))

}

class TString extends AAny {

  def getTyp = TString.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Gets the character at a specified index */
    case "at" =>
      val List(index) = parameters // Number
      Reporter.reportImprecision("String bounds are unchecked in this version of the analysis!",pp)
      Top[S](TString.typ) // TODO: Sound dummy

    /** Compares two pieces of text */
    case "compare" =>
      val List(other) = parameters // String
      Top[S](TNumber.typ) // TODO: Sound dummy

    /** Gets the length of the string */
    case "count" =>
      Top[S](TNumber.typ) // TODO: Sound dummy

    /** Concatenates two pieces of text */
    case "concat" =>
      Top[S](TString.typ) // TODO: Sound dummy

    /** Returns a value indicating if the second string is contained */
    case "contains" =>
      val List(value) = parameters // String
      Top[S](TBoolean.typ) // TODO: Sound dummy

    /** Stores text in the clipboard */
    case "copy_to_clipboard" =>
      Skip // TODO: Sound dummy

    /** Determines whether the ending matches the specified string */
    case "ends_with" =>
      val List(value) = parameters // String
      Top[S](TBoolean.typ) // TODO: Sound dummy

    /** Checks if two strings are the same */
    case "equals" =>
      val List(other) = parameters
      Return[S](this0 equal other)

    /** Returns the index of the first occurence if found starting at a given position */
    case "index_of" =>
      val List(value,start) = parameters // String,Number
      Top[S](TNumber.typ) // TODO: Sound dummy

    /** Inserts a string at a given position */
    case "insert" =>
      val List(start,value) = parameters // Number,String
      Top[S](TString.typ) // TODO: Sound dummy

    /** Indicates if the string is empty */
    case "is_empty" =>
      Top[S](TBoolean.typ) // TODO: Sound dummy

    /** Indicates if the string matches a regular expression */
    case "is_match_regex" =>
      val List(pattern) = parameters // String
      Top[S](TBoolean.typ) // TODO: Sound dummy

    /** Returns the index of the last occurence if found starting at a given position */
    case "last_index_of" =>
      val List(value,start) = parameters // String,Number
      TopWithInvalid[S](TNumber.typ) // TODO: Sound dummy

    /** Gets the strings matching the regex expression (pattern) */
    case "matches" =>
      val List(pattern) = parameters // String
      Top[S](TString_Collection.typ)

    /** Returns the string with characters removed starting at a given index */
    case "remove" =>
      val List(start) = parameters // Number
      Top[S](TString.typ) // TODO: Sound dummy

    /** Returns a given string with a replacement */
    case "replace" =>
      val List(old,newS) = parameters // String,String
      Top[S](TString.typ) // TODO: Sound dummy

    /** Replace every match of the regex according to the replacement string */
    case "replace_regex" =>
      val List(pattern,replace) = parameters // String,String
      Top[S](TString.typ) // TODO: Sound dummy

    /** Shares the string (email, sms, facebook, social or '' to pick from a list) */
    case "share" =>
      val List(network) = parameters // String
      Skip // TODO: Sound dummy

    /** Returns a string collection that contains the substrings in this string that are delimited by elements of a specified string. */
    case "split" =>
      val List(separator) = parameters // String
      Top[S](TString_Collection.typ) // TODO: Sound dummy

    /** Determines whether the beginning matches the specified string */
    case "starts_with" =>
      val List(value) = parameters // String
      Top[S](TBoolean.typ) // TODO: Sound dummy

    /** Returns a substring given a start index and a length */
    case "substring" =>
      val List(start,length) = parameters // Number,Number
      Top[S](TString.typ) // TODO: Sound dummy

    /** Parses the string as a boolean */
    case "to_boolean" =>
      TopWithInvalid[S](TBoolean.typ) // TODO: Sound dummy

    /** Parses the string as a color. */
    case "to_color" =>
      TopWithInvalid[S](TColor.typ) // TODO: Sound dummy

    /** Parses the string as a date and time. */
    case "to_datetime" =>
      TopWithInvalid[S](TDateTime.typ) // TODO: Sound dummy

    /** Parses the string as a geo coordinate. */
    case "to_location" =>
      TopWithInvalid[S](TLocation.typ) // TODO: Sound dummy

    /** Returns a copy of this string converted to lowercase, using the casing rules of the current culture. */
    case "to_lower_case" =>
      Top[S](TString.typ) // TODO: Sound dummy

    /** Parses the string as a number */
    case "to_number" =>
      TopWithInvalid[S](TNumber.typ)  // TODO: Sound dummy

    /** Parses the string as a time (12:30:12) and returns the number of seconds. */
    case "to_time" =>
      TopWithInvalid[S](TNumber.typ) // TODO: Sound dummy

    /** Converts a single character string into its unicode number */
    case "to_unicode" =>
      Top[S](TNumber.typ)  // TODO: Sound dummy

    /** Returns a copy of this string converted to uppercase, using the casing rules of the current culture. */
    case "to_upper_case" =>
      Top[S](TString.typ) // TODO: Sound dummy

    /** Removes all leading and trailing occurrences of a set of characters specified in a string from the current string. */
    case "trim" =>
      val List(chars) = parameters // String
      Top[S](TString.typ) // TODO: Sound dummy

    /** Removes all trailing occurrences of a set of characters specified in a string from the current string. */
    case "trim_end" =>
      val List(chars) = parameters // String
      Top[S](TString.typ) // TODO: Sound dummy

    /** Removes all leading occurrences of a set of characters specified in a string from the current string. */
    case "trim_start" =>
      val List(chars) = parameters // String
      Top[S](TString.typ) // TODO: Sound dummy

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
