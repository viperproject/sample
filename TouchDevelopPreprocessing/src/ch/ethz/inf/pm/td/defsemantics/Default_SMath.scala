/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Math
 *
 * Mathematical operators, cos, sin, ...
 *
 * @author Lucas Brutschy
 */

trait Default_SMath extends ASingleton {

  lazy val typeName = TypeName("Math", isSingleton = true)
          
  /** Frequently used: Returns the absolute value of a number */
  def member_abs = ApiMember(
    name = "abs",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Returns the angle whose cosine is the specified number */
  def member_acos = ApiMember(
    name = "acos",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Returns the angle whose sine is the specified number */
  def member_asin = ApiMember(
    name = "asin",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Returns the angle whose tangent is the specified number */
  def member_atan = ApiMember(
    name = "atan",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Returns the angle whose tangent is the quotient of two specified numbers */
  def member_atan2 = ApiMember(
    name = "atan2",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Returns the smallest integral value greater than or equal to the specified number */
  def member_ceiling = ApiMember(
    name = "ceiling",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Clamps `value` between `min` and `max` */
  def member_clamp = ApiMember(
    name = "clamp",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Frequently used: Returns the cosine of the specified angle (in radians) */
  def member_cos = ApiMember(
    name = "cos",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Returns the hyperbolic cosine of the specified angle (in radians) */
  def member_cosh = ApiMember(
    name = "cosh",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Creates a matrix of zeros of a given size */
  def member_create_matrix = ApiMember(
    name = "create matrix",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TMatrix,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Use Collections->create number map instead. */
  def member_create_number_map = ApiMember(
    name = "create number map",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber_Map,
    semantics = DefaultSemantics
  )

  /** Frequently used: Creates a 3D vector */
  def member_create_vector3 = ApiMember(
    name = "create vector3",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TVector3,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Converts degrees into radians */
  def member_deg_to_rad = ApiMember(
    name = "deg to rad",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Returns the result of integer division of one number by another number */
  def member_div = ApiMember(
    name = "div",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Returns the natural logarithmic base, specified by the constant, e */
  def member_e = ApiMember(
    name = "e",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Returns e raised to the specified power */
  def member_exp = ApiMember(
    name = "exp",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Frequently used: Returns the largest integer less than or equal to the specified number */
  def member_floor = ApiMember(
    name = "floor",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Returns the gravity constant (9.80665) */
  def member_gravity = ApiMember(
    name = "gravity",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Rarely used: Returns the remainder resulting from the division of a specified number by another specified number */
  def member_ieee_remainder = ApiMember(
    name = "ieee remainder",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Indicates that value cannot be represented as a number, i.e. Not-a-Number. This usually happens when the number is the result of a division by zero. */
  def member_is_nan = ApiMember(
    name = "is nan",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Indicates whether number evaluates to negative or positive infinity */
  def member_is_inf = ApiMember(
    name = "is ∞",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: Indicates whether number evaluates to positive infinity */
  def member_is_pos_inf = ApiMember(
    name = "is ∞₊",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: Indicates whether number evaluates to negative infinity */
  def member_is_neg_inf = ApiMember(
    name = "is ∞₋",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Rarely used: Returns the logarithm of a specified number in a specified base */
  def member_log = ApiMember(
    name = "log",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Returns the base 10 logarithm of a specified number */
  def member_log10 = ApiMember(
    name = "log10",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Frequently used: Returns the natural (base e) logarithm of a specified number */
  def member_loge = ApiMember(
    name = "loge",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Returns the larger of two numbers */
  def member_max = ApiMember(
    name = "max",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Returns the smaller of two numbers */
  def member_min = ApiMember(
    name = "min",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Frequently used: Returns the modulus resulting from the division of one number by another number */
  def member_mod = ApiMember(
    name = "mod",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Clamps `value` between 0 and 1. */
  def member_normalize = ApiMember(
    name = "normalize",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Frequently used: Returns a specified number raised to the specified power */
  def member_pow = ApiMember(
    name = "pow",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Converts rad into degrees */
  def member_rad_to_deg = ApiMember(
    name = "rad to deg",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Renamed to 'random normalized' */
  def member_rand_norm = ApiMember(
    name = "rand norm",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Returns a random floating-point number x: 0 ≤ x < 1 */
  def member_random_normalized = ApiMember(
    name = "random normalized",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Returns a random integral number between `min` and `max` included. */
  def member_random_range = ApiMember(
    name = "random range",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Frequently used: Returns a random integral number bounded between limit and 0, not including limit unless it is 0 */
  def member_random = ApiMember(
    name = "random",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Return numbers between `start` and `start + length - 1` inclusively */
  def member_range = ApiMember(
    name = "range",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = GCollection(TNumber),
    semantics = DefaultSemantics
  )

  /** Frequently used: Rounds a number to a specified number of fractional digits. */
  def member_round_with_precision = ApiMember(
    name = "round with precision",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Frequently used: Rounds a number to the nearest integral value */
  def member_round = ApiMember(
    name = "round",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Returns a value indicating the sign of a number */
  def member_sign = ApiMember(
    name = "sign",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Frequently used: Returns the sine of the specified angle (in radians) */
  def member_sin = ApiMember(
    name = "sin",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Returns the hyperbolic sine of the specified angle (in radians) */
  def member_sinh = ApiMember(
    name = "sinh",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Frequently used: Returns the square root of a specified number */
  def member_sqrt = ApiMember(
    name = "sqrt",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Returns the tangent of the specified angle (in radians) */
  def member_tan = ApiMember(
    name = "tan",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Returns the hyperbolic tangent of the specified angle (in radians) */
  def member_tanh = ApiMember(
    name = "tanh",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Returns the machine epsilon, the smallest positive number greater than zero. */
  def member_epsilon = ApiMember(
    name = "ε",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Frequently used: Returns the Pi constant */
  def member_pi = ApiMember(
    name = "π",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Rarely used: Returns the positive infinity */
  def member_pos_inf = ApiMember(
    name = "∞₊",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Returns the negative infinity */
  def member_neg_inf = ApiMember(
    name = "∞₋",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "abs" -> member_abs,
    "acos" -> member_acos,
    "asin" -> member_asin,
    "atan" -> member_atan,
    "atan2" -> member_atan2,
    "ceiling" -> member_ceiling,
    "clamp" -> member_clamp,
    "cos" -> member_cos,
    "cosh" -> member_cosh,
    "create matrix" -> member_create_matrix,
    "create number map" -> member_create_number_map,
    "create vector3" -> member_create_vector3,
    "deg to rad" -> member_deg_to_rad,
    "div" -> member_div,
    "e" -> member_e,
    "exp" -> member_exp,
    "floor" -> member_floor,
    "gravity" -> member_gravity,
    "ieee remainder" -> member_ieee_remainder,
    "is nan" -> member_is_nan,
    "is ∞" -> member_is_inf,
    "is ∞₊" -> member_is_pos_inf,
    "is ∞₋" -> member_is_neg_inf,
    "log" -> member_log,
    "log10" -> member_log10,
    "loge" -> member_loge,
    "max" -> member_max,
    "min" -> member_min,
    "mod" -> member_mod,
    "normalize" -> member_normalize,
    "pow" -> member_pow,
    "rad to deg" -> member_rad_to_deg,
    "rand norm" -> member_rand_norm,
    "random normalized" -> member_random_normalized,
    "random range" -> member_random_range,
    "random" -> member_random,
    "range" -> member_range,
    "round with precision" -> member_round_with_precision,
    "round" -> member_round,
    "sign" -> member_sign,
    "sin" -> member_sin,
    "sinh" -> member_sinh,
    "sqrt" -> member_sqrt,
    "tan" -> member_tan,
    "tanh" -> member_tanh,
    "ε" -> member_epsilon,
    "π" -> member_pi,
    "∞₊" -> member_pos_inf,
    "∞₋" -> member_neg_inf
  )
            

}
          
