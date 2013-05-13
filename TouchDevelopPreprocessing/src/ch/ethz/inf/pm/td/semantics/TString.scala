
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

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Gets the character at a specified index */
    case "at" =>
      val List(index) = parameters // Number
      Reporter.reportImprecision("String bounds are unchecked in this version of the analysis!",pp)
      Dummy[S](this0,method)
      Top[S](TString.typ)

    /** Compares two pieces of text */
    case "compare" =>
      val List(other) = parameters // String
      Dummy[S](this0,method)
      Top[S](TNumber.typ)

    /** Gets the length of the string */
    case "count" =>
      Dummy[S](this0,method)
      Top[S](TNumber.typ)

    /** Concatenates two pieces of text */
    case "concat" =>
      Dummy[S](this0,method)
      Top[S](TString.typ)

    /** Returns a value indicating if the second string is contained */
    case "contains" =>
      Dummy[S](this0,method)
      val List(value) = parameters // String
      Top[S](TBoolean.typ)

    /** Stores text in the clipboard */
    case "copy to clipboard" =>
      Dummy[S](this0,method)
      Skip

    /** Determines whether the ending matches the specified string */
    case "ends with" =>
      Dummy[S](this0,method)
      val List(value) = parameters // String
      Top[S](TBoolean.typ)

    /** Checks if two strings are the same */
    case "equals" =>
      val List(other) = parameters
      Return[S](this0 equal other)

    /** Returns the index of the first occurence if found starting at a given position */
    case "index of" =>
      Dummy[S](this0,method)
      val List(value,start) = parameters // String,Number
      Top[S](TNumber.typ) 

    /** Inserts a string at a given position */
    case "insert" =>
      Dummy[S](this0,method)
      val List(start,value) = parameters // Number,String
      Top[S](TString.typ) 

    /** Indicates if the string is empty */
    case "is empty" =>
      Dummy[S](this0,method)
      Top[S](TBoolean.typ) 

    /** Indicates if the string matches a regular expression */
    case "is match regex" =>
      Dummy[S](this0,method)
      val List(pattern) = parameters // String
      Top[S](TBoolean.typ) 

    /** Returns the index of the last occurence if found starting at a given position */
    case "last index of" =>
      Dummy[S](this0,method)
      val List(value,start) = parameters // String,Number
      TopWithInvalid[S](TNumber.typ) 

    /** Gets the strings matching the regex expression (pattern) */
    case "matches" =>
      Dummy[S](this0,method)
      val List(pattern) = parameters // String
      Top[S](TString_Collection.typ)

    /** Returns the string with characters removed starting at a given index */
    case "remove" =>
      Dummy[S](this0,method)
      val List(start) = parameters // Number
      Top[S](TString.typ) 

    /** Returns a given string with a replacement */
    case "replace" =>
      Dummy[S](this0,method)
      val List(old,newS) = parameters // String,String
      Top[S](TString.typ) 

    /** Replace every match of the regex according to the replacement string */
    case "replace regex" =>
      Dummy[S](this0,method)
      val List(pattern,replace) = parameters // String,String
      Top[S](TString.typ) 

    /** Shares the string (email, sms, facebook, social or '' to pick from a list) */
    case "share" =>
      Dummy[S](this0,method)
      val List(network) = parameters // String
      Skip 

    /** Returns a string collection that contains the substrings in this string that are delimited by elements of a specified string. */
    case "split" =>
      Dummy[S](this0,method)
      val List(separator) = parameters // String
      Top[S](TString_Collection.typ) 

    /** Determines whether the beginning matches the specified string */
    case "starts with" =>
      Dummy[S](this0,method)
      val List(value) = parameters // String
      Top[S](TBoolean.typ) 

    /** Returns a substring given a start index and a length */
    case "substring" =>
      Dummy[S](this0,method)
      val List(start,length) = parameters // Number,Number
      Top[S](TString.typ) 

    /** Parses the string as a boolean */
    case "to boolean" =>
      Dummy[S](this0,method)
      TopWithInvalid[S](TBoolean.typ) 

    /** Parses the string as a color. */
    case "to color" =>
      Dummy[S](this0,method)
      TopWithInvalid[S](TColor.typ) 

    /** Parses the string as a date and time. */
    case "to datetime" =>
      Dummy[S](this0,method)
      TopWithInvalid[S](TDateTime.typ) 

    /** Parses the string as a geo coordinate. */
    case "to location" =>
      Dummy[S](this0,method)
      TopWithInvalid[S](TLocation.typ) 

    /** Returns a copy of this string converted to lowercase, using the casing rules of the current culture. */
    case "to lower case" =>
      Dummy[S](this0,method)
      Top[S](TString.typ) 

    /** Parses the string as a number */
    case "to number" =>
      Dummy[S](this0,method)
      TopWithInvalid[S](TNumber.typ)  

    /** Parses the string as a time (12:30:12) and returns the number of seconds. */
    case "to time" =>
      Dummy[S](this0,method)
      TopWithInvalid[S](TNumber.typ) 

    /** Converts a single character string into its unicode number */
    case "to unicode" =>
      Dummy[S](this0,method)
      Top[S](TNumber.typ)  

    /** Returns a copy of this string converted to uppercase, using the casing rules of the current culture. */
    case "to upper case" =>
      Dummy[S](this0,method)
      Top[S](TString.typ) 

    /** Removes all leading and trailing occurrences of a set of characters specified in a string from the current string. */
    case "trim" =>
      Dummy[S](this0,method)
      val List(chars) = parameters // String
      Top[S](TString.typ) 

    /** Removes all trailing occurrences of a set of characters specified in a string from the current string. */
    case "trim end" =>
      Dummy[S](this0,method)
      val List(chars) = parameters // String
      Top[S](TString.typ) 

    /** Removes all leading occurrences of a set of characters specified in a string from the current string. */
    case "trim start" =>
      Dummy[S](this0,method)
      val List(chars) = parameters // String
      Top[S](TString.typ) 

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
