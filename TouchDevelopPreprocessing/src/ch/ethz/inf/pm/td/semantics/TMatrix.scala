
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{TouchField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._
import ch.ethz.inf.pm.td.semantics.TNumber_Collection._

/**
 * Specifies the abstract semantics of Matrix
 *
 * A 2D matrix of numbers
 *
 * @author Lucas Brutschy
 */ 

object TMatrix extends AMutable_Collection {

  /** Gets the number of columns */
  lazy val field_column_count = new TouchField("column count",TNumber.typeName)

  /** Gets the number of rows */
  lazy val field_row_count = new TouchField("row count",TNumber.typeName)

  lazy val typeName = TypeName("Matrix")

  def keyTypeName = TNumber.typeName

  def valueTypeName = TNumber.typeName

  override def possibleFields = super.possibleFields ++ Set(field_column_count,field_row_count)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {


    /** Creates a deep copy of the matrix. */
    case "clone" =>
      Clone[S](this0,recursive = true)

    /** Gets the value at a given location. Returns invalid if outside of the array dimensions */
    case "at2" =>
      val List(row,column) = parameters // Number,Number
      super.forwardSemantics[S](this0,"at",List(row*Field[S](this0,TMatrix.field_column_count)+column),returnedType)

    /** Sets the value at a particular position. If matrix  will be expanded if the position falls outside the boundaries. */
    case "set at2" =>
      val List(row,column,value) = parameters // Number,Number,Number
      super.forwardSemantics[S](this0,"set at",List(row*Field[S](this0,TMatrix.field_column_count)+column,value),returnedType)

    /** Computes the minimum of the values */
    case "min" =>
      Return[S](collectionAllValues[S](this0))

    /** Computes the maximum of the values */
    case "max" =>
      Return[S](collectionAllValues[S](this0))

    /** Returns a copy of the matrix scaled by factor. */
    // case "scale" => 
    //   val List(factor) = parameters // Number
    //   TopWithInvalid[S](TMatrix)
    // DECLARATION AS FIELD: 
    //   /** Returns a copy of the matrix scaled by factor. */
    //   lazy val field_scale = new TouchField("scale",TMatrix.typeName)

    /** Returns the matrix negated. */
    // case "negate" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TMatrix)
    // DECLARATION AS FIELD: 
    //   /** Returns the matrix negated. */
    //   lazy val field_negate = new TouchField("negate",TMatrix.typeName)

    /** Returns the transposed matrix. */
    // case "transpose" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TMatrix)
    // DECLARATION AS FIELD: 
    //   /** Returns the transposed matrix. */
    //   lazy val field_transpose = new TouchField("transpose",TMatrix.typeName)

    /** Returns a matrix resulting from adding this matrix to b. The size of both matrices must match. */
    // case "add" => 
    //   val List(b) = parameters // Matrix
    //   TopWithInvalid[S](TMatrix)
    // DECLARATION AS FIELD: 
    //   /** Returns a matrix resulting from adding this matrix to b. The size of both matrices must match. */
    //   lazy val field_add = new TouchField("add",TMatrix.typeName)

    /** Returns a matrix resulting from substracting b from this matrix. The size of both matrices must match. */
    // case "substract" => 
    //   val List(b) = parameters // Matrix
    //   TopWithInvalid[S](TMatrix)
    // DECLARATION AS FIELD: 
    //   /** Returns a matrix resulting from substracting b from this matrix. The size of both matrices must match. */
    //   lazy val field_substract = new TouchField("substract",TMatrix.typeName)

    /** Returns a matrix resulting from multiply each element in the matrices. The size of both matrices must match. */
    // case "multiply" =>
    //   val List(b) = parameters // Matrix
    //   TopWithInvalid[S](TMatrix)
    // DECLARATION AS FIELD: 
    //   /** Returns a matrix resulting from multiply each element in the matrices. The size of both matrices must match. */
    //   lazy val field_multiply = new TouchField("multiply",TMatrix.typeName)

    // FIELDS: field_count, field_row_count, field_column_count, field_at, field_at2, field_clone, field_min, field_max, field_scale, field_negate, field_transpose, field_add, field_substract, field_multiply, field_to_string, field_random

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
