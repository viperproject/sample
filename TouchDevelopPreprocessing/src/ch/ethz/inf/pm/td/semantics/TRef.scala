
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Ref
 *
 * A reference to a value
 *
 * @author Lucas Brutschy
 */

object TRef {

  val typName = "Ref"
  val typ = new TouchType(typName)

}

class TRef extends AAny {

  def getTyp = TRef.typ

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Add specified value to given reference */
    // case "◈add" => 
    //   val List(v) = parameters // Number
    //   Skip

    /** Set reference to invalid */
    // case "◈clear" => 
    //   val List() = parameters // 
    //   Skip

    /** Check if reference has been written to the storage/server */
    // case "◈confirmed" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TBoolean.typ)
    // DECLARATION AS FIELD: 
    //   /** Check if reference has been written to the storage/server */
    //   val field_◈confirmed = new TouchField("◈confirmed",TBoolean.typName)

    /** Get the current value of the reference */
    // case "◈get" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TT.typ)
    // DECLARATION AS FIELD: 
    //   /** Get the current value of the reference */
    //   val field_◈get = new TouchField("◈get",TT.typName)

    /** Retrive the reference itself (useful on globals and fields) */
    // case "◈ref" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](T{"g":"Ref","a":["T"]}.typ)
    // DECLARATION AS FIELD: 
    //   /** Retrive the reference itself (useful on globals and fields) */
    //   val field_◈ref = new TouchField("◈ref",T{"g":"Ref","a":["T"]}.typName)

    /** Set the value of the reference */
    // case "◈set" => 
    //   val List(t) = parameters // T
    //   Skip

    /** Set reference to `v` if it's currently non-empty */
    // case "◈test and set" => 
    //   val List(v) = parameters // T
    //   Skip

    // FIELDS: field_◈confirmed, field_◈get, field_◈ref

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
