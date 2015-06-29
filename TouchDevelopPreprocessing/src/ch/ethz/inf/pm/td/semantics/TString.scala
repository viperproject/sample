
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.defsemantics.Default_TString
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

object TString extends Default_TString {


  /** Never used: Clears the values from the map */
  def member_at_index = ApiMember(
    name = "at index",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = this,
    semantics = ValidPureSemantics
  )

  /** Never used: Clears the values from the map */
  def member_copy = ApiMember(
    name = "copy",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = this,
    semantics = DefaultSemantics
  )

  /** Returns the number of characters */
  //lazy val field_count = new TouchField("count",TNumber.typeName)

  override def possibleFields = super.possibleFields ++ List(
    // field_count
  )

  override def declarations = super.declarations ++ Map (
    "copy" -> member_copy, "at index" -> member_at_index
  )

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Gets the character at a specified index */
    case "at" =>
      val List(index) = parameters // Number
      Top[S](TString)

    /** Gets the character at a specified index */
    case "at index" =>
      val List(index) = parameters // Number
      Top[S](TString)

    /** Compares two pieces of text */
    case "compare" =>
      val List(other) = parameters // String
      Top[S](TNumber)

    /** Gets the length of the string */
    case "count" =>
      Return[S](0 ndToIncl PositiveInfinity)

    /** Concatenates two pieces of text */
    case "concat" =>
      Top[S](TString)

    /** Returns a value indicating if the second string is contained */
    case "contains" =>
      val List(value) = parameters // String
      Top[S](TBoolean)

    /** Concatenates two pieces of text */
    case "copy" =>
      Return[S](this0)
      //Clone[S](this0)

    /** Stores text in the clipboard */
    case "copy to clipboard" =>
      Skip

    /** Determines whether the ending matches the specified string */
    case "ends with" =>
      val List(value) = parameters // String
      Top[S](TBoolean)

    /** Checks if two strings are the same */
    case "equals" =>
      val List(other) = parameters
      val ret = Return[S](this0 equal other)
      ret

    /** Returns the index of the first occurence if found starting at a given position */
    case "index of" =>
      val List(value, start) = parameters // String,Number
      Top[S](TNumber)

    /** Inserts a string at a given position */
    case "insert" =>
      val List(start, value) = parameters // Number,String
      Top[S](TString)

    /** Indicates if the string is empty */
    case "is empty" =>
      Top[S](TBoolean)

    /** Indicates if the string matches a regular expression */
    case "is match regex" =>
      val List(pattern) = parameters // String
      Top[S](TBoolean)

    /** Returns the index of the last occurence if found starting at a given position */
    case "last index of" =>
      val List(value, start) = parameters // String,Number
      Top[S](TNumber)

    /** Gets the groups from the matching the regex expression (pattern) */
    case "match" =>
      val List(pattern) = parameters // String
      Top[S](GCollection(TString))

    /** Gets the strings matching the regex expression (pattern) */
    case "matches" =>
      val List(pattern) = parameters // String
      Top[S](GCollection(TString))

    /** Returns the string with characters removed starting at a given index */
    case "remove" =>
      val List(start) = parameters // Number
      Top[S](TString)

    /** Returns a given string with a replacement */
    case "replace" =>
      val List(old, newS) = parameters // String,String
      Top[S](TString)

    /** Replace every match of the regex according to the replacement string */
    case "replace regex" =>
      val List(pattern, replace) = parameters // String,String
      Top[S](TString)

    /** Shares the string (email, sms, facebook, social or '' to pick from a list) */
    case "share" =>
      val List(network) = parameters // String
      Skip

    /** Returns a string collection that contains the substrings in this string that are delimited by elements of a specified string. */
    case "split" =>
      val List(separator) = parameters // String

      // This is a hack to support constant split expressions
      (this0.getSingle,separator.getSingle) match {
        case (Some(Constant(str,_,_)),Some(Constant(sep,_,_))) =>

          val typ = GCollection(TString)
          val coll = str.split(sep)
          var curState = state
          curState = New[S](typ)(curState,pp)
          val absColl = curState.expr
          for (c <- coll) {
            curState = typ.Insert[S](absColl,typ.Count[S](absColl), Constant(c,TString,pp))(curState,pp)
            curState = typ.IncreaseLength[S](absColl)(curState,pp)
          }
          curState.setExpression(absColl)

        case _ =>

          // No matter what the arguments are, the resulting set has at least one element!
          var curState = state
          curState = Top[S](GCollection(TString))(curState, pp)
          val obj = curState.expr
          curState = Assume(GCollection(TString).Count[S](obj) >= 1)(curState, pp)
          Return[S](obj)(curState, pp)

      }

    /** Determines whether the beginning matches the specified string */
    case "starts with" =>
      val List(value) = parameters // String
      Top[S](TBoolean)

    /** Returns a substring given a start index and a length */
    case "substring" =>
      val List(start, length) = parameters // Number,Number
      Top[S](TString)

    /** Parses the string as a boolean */
    case "to boolean" =>
      TopWithInvalid[S](TBoolean, "conversion to boolean may fail")

    /** Parses the string as a color. */
    case "to color" =>
      TopWithInvalid[S](TColor, "conversion to color may fail")

    /** Parses the string as a date and time. */
    case "to datetime" =>
      TopWithInvalid[S](TDateTime, "conversion to datetime may fail")

    /** Parses the string as a geo coordinate. */
    case "to location" =>
      TopWithInvalid[S](TLocation, "conversion to location may fail")

    /** Returns a copy of this string converted to lowercase, using the casing rules of the current culture. */
    case "to lower case" =>
      Top[S](TString)

    /** Parses the string as a number */
    case "to number" =>
      Top[S](TNumber)
    // FIXME: UNSOUNDNESS: TopWithInvalid[S](TNumber)

    /** Parses the string as a time (12:30:12) and returns the number of seconds. */
    case "to time" =>
      TopWithInvalid[S](TNumber, "conversion to time may fail")

    /** Converts a single character string into its unicode number */
    case "to unicode" =>
      Top[S](TNumber)

    /** Returns a copy of this string converted to uppercase, using the casing rules of the current culture. */
    case "to upper case" =>
      Top[S](TString)

    /** Removes all leading and trailing occurrences of a set of characters specified in a string from the current string. */
    case "trim" =>
      val List(chars) = parameters // String
      Top[S](TString)

    /** Removes all trailing occurrences of a set of characters specified in a string from the current string. */
    case "trim end" =>
      val List(chars) = parameters // String
      Top[S](TString)

    /** Removes all leading occurrences of a set of characters specified in a string from the current string. */
    case "trim start" =>
      val List(chars) = parameters // String
      Top[S](TString)

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
