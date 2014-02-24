package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.NumericalAnalysisConstants
import ch.ethz.inf.pm.td.analysis.TouchAnalysisParameters

/**
 * User: lucas
 * Date: 11/8/12
 * Time: 6:53 PM
 */

object SMath {

  val typName = "Math"
  val typ = DefaultTouchType(typName, isSingleton = true)

}

class SMath extends AAny {

  def getTyp = SMath.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)(implicit pp:ProgramPoint,state:S):S = method match {

    /** Returns the negative infinity */
    case "∞₋" =>
      Top[S](TNumber.typ) // TODO

    /** Returns the positive infinity */
    case "∞₊" =>
      Top[S](TNumber.typ) // TODO

    /** Returns the absolute value of a number */
    case "abs" =>
      val List(x) = parameters // Number
      Top[S](TNumber.typ) // TODO

    /** Returns the angle whose cosine is the specified number */
    case "acos" =>
      val List(x) = parameters // Number
      Top[S](TNumber.typ) // TODO

    /** Returns the angle whose sine is the specified number */
    case "asin" =>
      val List(x) = parameters // Number
      Top[S](TNumber.typ) // TODO

    /** Returns the angle whose tangent is the specified number */
    case "atan" =>
      val List(x) = parameters // Number
      Top[S](TNumber.typ) // TODO

    /** Returns the angle whose tangent is the quotient of two specified numbers */
    case "atan2" =>
      val List(y,x) = parameters // Number,Number
      Top[S](TNumber.typ) // TODO

    /** Returns the smallest integral value greater than or equal to the specified number */
    case "ceiling" =>
      val List(x) = parameters // Number
      Top[S](TNumber.typ) // TODO

    /** Clamps `value` between `min` and `max` */
    case "clamp" =>
      val List(min, max, value) = parameters // Number,Number,Number
      If[S](value <= min, Then = (state) => {
        Return[S](min)(state, pp)
      }, Else = (state) => {
        If[S](value >= max, Then = (state2) => {
          Return[S](max)(state2, pp)
        }, Else = (state2) => {
          Return[S](value)(state2, pp)
        })(state, pp)
      })

    /** Returns the cosine of the specified angle (in radians) */
    case "cos" =>
      val List(angle) = parameters // Number
      Return[S](-1 ndTo 1)

    /** Returns the hyperbolic cosine of the specified angle (in radians) */
    case "cosh" =>
      val List(angle) = parameters // Number
      Top[S](TNumber.typ) // TODO

    /** OBSOLETE: Creates an empty number collection */
    case "create number collection" =>
      New[S](TNumber_Collection.typ)

    /** OBSOLETE: Creates an empty number map */
    case "create number map" =>
      New[S](TNumber_Map.typ)

    /** Creates a 3D vector */
    case "create vector3" =>
      val List(x,y,z) = parameters // Number,Number,Number
      New[S](TVector3.typ,Map(
        TVector3.field_x -> x,
        TVector3.field_y -> y,
        TVector3.field_z -> z
      ))

    /** Converts degrees into radians */
    case "deg to rad" =>
      val List(degrees) = parameters // Number
      Top[S](TNumber.typ) // TODO

    /** Returns the natural logarithmic base, specified by the constant, e */
    case "e" =>
      Top[S](TNumber.typ) // TODO

    /** Returns e raised to the specified power */
    case "exp" =>
      val List(x) = parameters // Number
      Top[S](TNumber.typ) // TODO

    /** Returns the largest integer less than or equal to the specified number */
    case "floor" =>
      val List(x) = parameters // Number
      Top[S](TNumber.typ) // TODO

    /** Returns the gravity constant (9.80665) */
    case "gravity" =>
      Return(toRichExpression(9.80665))

    /** Returns the remainder resulting from the division of a specified number by another specified number */
    case "ieee remainder" =>
      val List(x,y) = parameters // Number,Number
      Top[S](TNumber.typ) // TODO

    /** Indicates whether number evaluates to negative or positive infinity */
    case "is ∞" =>
      val List(x) = parameters // Number
      Top[S](TBoolean.typ) // TODO

    /** Indicates whether number evaluates to negative infinity */
    case "is ∞₋" =>
      val List(x) = parameters // Number
      Top[S](TBoolean.typ) // TODO

    /** Indicates whether number evaluates to positive infinity */
    case "is ∞₊" =>
      val List(x) = parameters // Number
      Top[S](TBoolean.typ) // TODO

    /** Indicates that value cannot be represented as a number, i.e. Not-a-Number. This usually happens when the number is the initial of a division by zero. */
    case "is nan" =>
      val List(x) = parameters // Number
      Top[S](TBoolean.typ) // TODO

    /** Returns the logarithm of a specified number in a specified base */
    case "log" =>
      val List(x,base) = parameters // Number,Number
      Top[S](TNumber.typ) // TODO

    /** Returns the base 10 logarithm of a specified number */
    case "log10" =>
      val List(x) = parameters // Number
      Top[S](TNumber.typ) // TODO

    /** Returns the natural (base e) logarithm of a specified number */
    case "loge" =>
      val List(x) = parameters // Number
      Top[S](TNumber.typ) // TODO

    /** Returns the larger of two numbers */
    case "max" =>
      val List(x,y) = parameters // Number,Number
      Top[S](TNumber.typ) // TODO

    /** Returns the smaller of two numbers */
    case "min" =>
      val List(x,y) = parameters // Number,Number
      Top[S](TNumber.typ) // TODO

    /** Returns the modulus resulting from the division of one number by another number */
    case "mod" =>
      val List(x,y) = parameters // Number,Number
      Top[S](TNumber.typ) // TODO

    /** Clamps `value` between 0 and 1. */
    case "normalize" =>
      val List(value) = parameters // Number
      If[S](value <= 0, Then = (state) => {
        Return[S](0)(state, pp)
      }, Else = (state) => {
        If[S](value >= 1, Then = (state2) => {
          Return[S](1)(state2, pp)
        }, Else = (state2) => {
          Return[S](value)(state2, pp)
        })(state, pp)
      })

    /** Returns a specified number raised to the specified power */
    case "pow" =>
      val List(x,y) = parameters // Number,Number
      Top[S](TNumber.typ) // TODO

    /** Converts rad into degrees */
    case "rad to deg" =>
      val List(radians) = parameters // Number
      Return[S]((radians / math.Pi)*180)

    case "rand" =>
      val List(upperBound) = parameters
      Return[S](toRichExpression(0) ndTo (upperBound - 1))

    /** Returns a random integral number x bounded between limit and 0, not including limit unless it is 0 */
    case "random" =>
      val List(upperBound) = parameters
      val res = Return[S](toRichExpression(0) ndTo (upperBound - 1))
      res

    /** Returns a random floating-point number x: 0 â‰¤ x < 1 */
    case "rand norm" =>
      Return[S](toRichExpression(0) ndTo toRichExpression(1-NumericalAnalysisConstants.epsilon))

    /** Returns a random floating-point number x: 0 â‰¤ x < 1 */
    case "random normalized" =>
      Return[S](toRichExpression(0) ndTo toRichExpression(1-NumericalAnalysisConstants.epsilon))

    /** Returns a random integral number between `min` and `max` included. */
    case "random range" =>
      val List(min,max) = parameters // Number,Number
      Return[S](min ndTo max)

    /** Rounds a number to the nearest integral value */
    case "round" =>
      val List(x) = parameters // Number
      Return[S](x-0.5 ndTo x+0.5)

    /** Rounds a number to a specified number of fractional digits. */
    case "round with precision" =>
      val List(x,digits) = parameters // Number,Number
      Return[S](x-0.5 ndTo x+0.5)

    /** Returns a value indicating the sign of a number */
    case "sign" =>
      val List(x) = parameters // Number
      Top[S](TNumber.typ) // TODO

    /** Returns the sine of the specified angle (in radians) */
    case "sin" =>
      val List(angle) = parameters // Number
      Return[S](-1 ndTo 1)

    /** Returns the hyperbolic sine of the specified angle (in radians) */
    case "sinh" =>
      val List(angle) = parameters // Number
      Top[S](TNumber.typ) // TODO

    /** Returns the square root of a specified number */
    case "sqrt" =>
      val List(x) = parameters // Number
      if (TouchAnalysisParameters.reportNumericalErrors)
        Error[S](x < 0, "sqrt", "Might compute the square root of a negative number")
      Return[S]((0 ndTo x),(x ndTo 1)) // PRECISION: This is very rough

    /** Returns the tangent of the specified angle (in radians) */
    case "tan" =>
      val List(angle) = parameters // Number
      Top[S](TNumber.typ) // TODO

    /** Returns the hyperbolic tangent of the specified angle (in radians) */
    case "tanh" =>
      val List(angle) = parameters // Number
      Top[S](TNumber.typ) // TODO

    /** Returns the smallest positive number greater than zero. */
    case "ε" =>
      Top[S](TNumber.typ) // TODO

    /** Returns the Pi constant */
    case "π" =>
      Return[S](toRichExpression(3.14159)) // TODO: PRECISION?

    /** Creates a matrix of zeros of a given size */
    case "create matrix" =>
      val List(rows,columns) = parameters // Number,Number
      New[S](TMatrix.typ,Map(
        TMatrix.field_column_count -> columns,
        TMatrix.field_row_count -> rows
      ),
        initialCollectionSize = Some(toExpressionSet(rows * columns)),
        initialCollectionValue = Some(toExpressionSet(0))
      )

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}