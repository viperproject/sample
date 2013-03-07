
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

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
  val typ = new TouchType(typName,isSingleton = false,List(/*TODO field_count*/))

}

class TString extends AAny {

  def getTyp = TString.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Gets the character at a specified index */
    case "at" =>
      val List(index) = parameters // Number
      //TODO CheckInRangeInclusive[S](index,0,Field[S](this0,TString.field_count) - 1,method,"index")
      New[S](TString.typ)

    /** Compares two pieces of text */
    // case "compare" => 
    //   val List(other) = parameters // String
    //   Top[S](TNumber.typ)


    /** Gets the length of the string */
    case "count" =>
      Top[S](TNumber.typ)

    /** Concatenates two pieces of text */
    case "concat" =>
      //TODO val List(other) = parameters // String
      //TODO val state1 = New[S](TString.typ)
      //TODO val newString = state1.getExpression()
      //TODO val state2 = AssignField[S](newString,TString.field_count,
      //TODO   Field[S](this0,TString.field_count) + Field[S](other,TString.field_count))(state1,pp)
      Top[S](TString.typ)//TODO (state2,pp)

    /** Returns a value indicating if the second string is contained */
    case "contains" =>
      val List(value) = parameters // String
      Return[S](True,False)

    /** Stores text in the clipboard */
    case "copy_to_clipboard" =>
      Skip

    /** Determines whether the ending matches the specified string */
    // case "ends_with" => 
    //   val List(value) = parameters // String
    //   Top[S](TBoolean.typ)

    /** Checks if two strings are the same */
    case "equals" =>
      val List(other) = parameters
      //TODO Return[S]((Field[S](this0,TString.field_count) equal Field[S](other,TString.field_count)),False)
      Top[S](TBoolean.typ)

    /** Returns the index of the first occurence if found starting at a given position */
    case "index_of" =>
      val List(value,start) = parameters // String,Number
      //TODO Return[S]((start ndTo (Field[S](this0,TString.field_count) - 1)),Invalid(TNumber.typ))
      Top[S](TNumber.typ)

    /** Inserts a string at a given position */
    // case "insert" => 
    //   val List(start,value) = parameters // Number,String
    //   Top[S](TString.typ)

    /** Indicates if the string is empty */
    case "is_empty" =>
      //TODO Return[S](Field[S](this0,TString.field_count) equal 0)
      Top[S](TBoolean.typ)

    /** Indicates if the string matches a regular expression */
    // case "is_match_regex" => 
    //   val List(pattern) = parameters // String
    //   Top[S](TBoolean.typ)

    /** Returns the index of the last occurence if found starting at a given position */
    // case "last_index_of" => 
    //   val List(value,start) = parameters // String,Number
    //   Top[S](TNumber.typ)

    /** Gets the strings matching the regex expression (pattern) */
    case "matches" =>
      val List(pattern) = parameters // String
      Top[S](TString_Collection.typ)

    /** Returns the string with characters removed starting at a given index */
    // case "remove" => 
    //   val List(start) = parameters // Number
    //   Top[S](TString.typ)

    /** Returns a given string with a replacement */
    case "replace" =>
       val List(old,newS) = parameters // String,String
       //TODO Error[S](Field[S](old,TString.field_count) < 1,"replace","The string to be replaced might be empty")
       New[S](TString.typ)

    /** Replace every match of the regex according to the replacement string */
    // case "replace_regex" => 
    //   val List(pattern,replace) = parameters // String,String
    //   Top[S](TString.typ)

    /** Shares the string (email, sms, facebook, social or '' to pick from a list) */
    // case "share" => 
    //   val List(network) = parameters // String
    //   Skip;

    /** Returns a string collection that contains the substrings in this string that are delimited by elements of a specified string. */
    case "split" =>
      val List(separator) = parameters // String
      Top[S](TString_Collection.typ)

    /** Determines whether the beginning matches the specified string */
    // case "starts_with" => 
    //   val List(value) = parameters // String
    //   Top[S](TBoolean.typ)

    /** Returns a substring given a start index and a length */
    // case "substring" => 
    //   val List(start,length) = parameters // Number,Number
    //   Top[S](TString.typ)

    /** Parses the string as a boolean */
    // case "to_boolean" => 
    //   Top[S](TBoolean.typ)
    // DECLARATION AS FIELD: 
    //   /** Parses the string as a boolean */
    //   val field_to_boolean = new TouchField("to_boolean",TBoolean.typ)

    /** Parses the string as a color. */
    // case "to_color" => 
    //   Top[S](TColor.typ)
    // DECLARATION AS FIELD: 
    //   /** Parses the string as a color. */
    //   val field_to_color = new TouchField("to_color",TColor.typ)

    /** Parses the string as a date and time. */
    // case "to_datetime" => 
    //   Top[S](TDateTime.typ)
    // DECLARATION AS FIELD: 
    //   /** Parses the string as a date and time. */
    //   val field_to_datetime = new TouchField("to_datetime",TDateTime.typ)

    /** Parses the string as a geo coordinate. */
    // case "to_location" => 
    //   Top[S](TLocation.typ)
    // DECLARATION AS FIELD: 
    //   /** Parses the string as a geo coordinate. */
    //   val field_to_location = new TouchField("to_location",TLocation.typ)

    /** Returns a copy of this string converted to lowercase, using the casing rules of the current culture. */
    // case "to_lower_case" => 
    //   Top[S](TString.typ)
    // DECLARATION AS FIELD: 
    //   /** Returns a copy of this string converted to lowercase, using the casing rules of the current culture. */
    //   val field_to_lower_case = new TouchField("to_lower_case",TString.typ)

    /** Parses the string as a number */
    // case "to_number" => 
    //   Top[S](TNumber.typ)
    // DECLARATION AS FIELD: 
    //   /** Parses the string as a number */
    //   val field_to_number = new TouchField("to_number",TNumber.typ)

    /** Parses the string as a time (12:30:12) and returns the number of seconds. */
    // case "to_time" => 
    //   Top[S](TNumber.typ)
    // DECLARATION AS FIELD: 
    //   /** Parses the string as a time (12:30:12) and returns the number of seconds. */
    //   val field_to_time = new TouchField("to_time",TNumber.typ)

    /** Converts a single character string into its unicode number */
    // case "to_unicode" => 
    //   Top[S](TNumber.typ)
    // DECLARATION AS FIELD: 
    //   /** Converts a single character string into its unicode number */
    //   val field_to_unicode = new TouchField("to_unicode",TNumber.typ)

    /** Returns a copy of this string converted to uppercase, using the casing rules of the current culture. */
    // case "to_upper_case" => 
    //   Top[S](TString.typ)
    // DECLARATION AS FIELD: 
    //   /** Returns a copy of this string converted to uppercase, using the casing rules of the current culture. */
    //   val field_to_upper_case = new TouchField("to_upper_case",TString.typ)

    /** Removes all leading and trailing occurrences of a set of characters specified in a string from the current string. */
    // case "trim" => 
    //   val List(chars) = parameters // String
    //   Top[S](TString.typ)

    /** Removes all trailing occurrences of a set of characters specified in a string from the current string. */
    // case "trim_end" => 
    //   val List(chars) = parameters // String
    //   Top[S](TString.typ)

    /** Removes all leading occurrences of a set of characters specified in a string from the current string. */
    // case "trim_start" => 
    //   val List(chars) = parameters // String
    //   Top[S](TString.typ)

    // FIELDS: , field_count, field_is_empty, field_to_boolean, field_to_color, field_to_datetime, field_to_location, field_to_lower_case, field_to_number, field_to_time, field_to_unicode, field_to_upper_case

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
