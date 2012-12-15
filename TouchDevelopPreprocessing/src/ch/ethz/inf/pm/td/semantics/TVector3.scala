package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{State, ExpressionSet}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import invoke.CallSite

/**
 * User: lucas
 * Date: 11/8/12
 * Time: 6:10 PM
 */
object TVector3 {

  val field_x = new TouchField("x", TNumber.typ)
  val field_y = new TouchField("y", TNumber.typ)
  val field_z = new TouchField("z", TNumber.typ)

  val typName = "Vector3"
  val typ = TouchType(typName,isSingleton = false, List(field_x,field_y,field_z))

}


class TVector3 extends AAny {

  def getTyp = TVector3.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S = method match {

    /** Adds a vector */
    case "add" =>
      val List(other) = parameters // Vector3
      val x = Field[S](this0,TVector3.field_x) + Field[S](other,TVector3.field_x)
      val y = Field[S](this0,TVector3.field_y) + Field[S](other,TVector3.field_y)
      val z = Field[S](this0,TVector3.field_z) + Field[S](other,TVector3.field_z)
      New[S](TVector3.typ,x,y,z)

    /** Restricts the vector in the specified range */
    case "clamp" =>
      val List(min,max) = parameters // Vector3,Vector3
      val x = Field[S](min,TVector3.field_x) ndTo Field[S](max,TVector3.field_x)
      val y = Field[S](min,TVector3.field_y) ndTo Field[S](max,TVector3.field_y)
      val z = Field[S](min,TVector3.field_z) ndTo Field[S](max,TVector3.field_z)
      // PRECISION: Here we are not using the original value of the vector.
      New[S](TVector3.typ,x,y,z)

    /** Calculates the cross product with the other vector */
    case "cross" =>
      val List(other) = parameters // Vector3
      val thisX = Field[S](this0,TVector3.field_x)
      val thisY = Field[S](this0,TVector3.field_y)
      val thisZ = Field[S](this0,TVector3.field_z)
      val otherX = Field[S](other,TVector3.field_x)
      val otherY = Field[S](other,TVector3.field_y)
      val otherZ = Field[S](other,TVector3.field_z)
      val x = thisY * otherZ - thisZ * otherY
      val y = thisZ * otherX - thisX * otherZ
      val z = thisX * otherY - thisY * otherX
      New[S](TVector3.typ,x,y,z)

    /** Gets the distance between the two vectors */
    case "distance" =>
      val List(other) = parameters // Vector3
      val x = Field[S](this0,TVector3.field_x) - Field[S](other,TVector3.field_x)
      val y = Field[S](this0,TVector3.field_y) - Field[S](other,TVector3.field_y)
      val z = Field[S](this0,TVector3.field_z) - Field[S](other,TVector3.field_z)
      val disSquare = x * x + y * y + z * z
      val distance = 0 ndTo (disSquare or 1)
      // PRECISION: Imprecise square root
      Return(distance)

    /** Gets the length of the vector */
    case "length" =>
      val thisX = Field[S](this0,TVector3.field_x)
      val thisY = Field[S](this0,TVector3.field_y)
      val thisZ = Field[S](this0,TVector3.field_z)
      val lenSquare = thisX * thisX + thisY * thisY + thisZ * thisZ
      val length = 0 ndTo (lenSquare or 1)
      // PRECISION: Imprecise square root
      Return(length)

    /** Linear interpolation between two vectors */
    case "linear_interpolation" =>
      val List(other,amount) = parameters // Vector3,Number
      val x = Field[S](this0,TVector3.field_x) + amount * Field[S](other,TVector3.field_x)
      val y = Field[S](this0,TVector3.field_y) + amount * Field[S](other,TVector3.field_y)
      val z = Field[S](this0,TVector3.field_z) + amount * Field[S](other,TVector3.field_z)
      New[S](TVector3.typ, x, y, z)

    /** Multiplies component-wise with a vector */
    case "multiply" =>
      val List(other) = parameters // Vector3
      val x = Field[S](this0,TVector3.field_x) * Field[S](other,TVector3.field_x)
      val y = Field[S](this0,TVector3.field_y) * Field[S](other,TVector3.field_y)
      val z = Field[S](this0,TVector3.field_z) * Field[S](other,TVector3.field_z)
      New[S](TVector3.typ, x, y, z)

    /** Returns a vector pointing in the opposite direction */
    case "negate" =>
      val x = 0 - Field[S](this0,TVector3.field_x)
      val y = 0 - Field[S](this0,TVector3.field_y)
      val z = 0 - Field[S](this0,TVector3.field_z)
      New[S](TVector3.typ, x, y, z)

    /** Returns a vector of one unit pointing in the same direction as the original vector */
    case "normalize" =>
      // PRECISION: There is a more precise way to do this
      New[S](TVector3.typ, -1 ndTo 1, -1 ndTo 1, -1 ndTo 1)

    /** Multiplies with a scaling factor */
    case "scale" =>
      val List(scalar) = parameters // Number
      val x = scalar * Field[S](this0,TVector3.field_x)
      val y = scalar * Field[S](this0,TVector3.field_y)
      val z = scalar * Field[S](this0,TVector3.field_z)
      New[S](TVector3.typ,x,y,z)

    /** Subtracts another vector */
    case "subtract" =>
      val List(other) = parameters // Vector3
      val x = Field[S](this0,TVector3.field_x) - Field[S](other,TVector3.field_x)
      val y = Field[S](this0,TVector3.field_y) - Field[S](other,TVector3.field_y)
      val z = Field[S](this0,TVector3.field_z) - Field[S](other,TVector3.field_z)
      New[S](TVector3.typ,x,y,z)

    /** Turns the vector into a string */
    case "to_string" =>
      Return(Valid(TString.typ))

    case _ =>
      super.forwardSemantics(this0,method,parameters)
  }
}