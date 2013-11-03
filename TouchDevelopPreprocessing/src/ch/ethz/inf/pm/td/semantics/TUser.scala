
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of User
 *
 * A user account
 *
 * @author Lucas Brutschy
 */ 

object TUser {

  /** Gets the about-me text of the user */
  val field_about = new TouchField("about",TString.typName)

  /** Indicates if the user has a picture */
  val field_has_picture = new TouchField("has picture",TBoolean.typName)

  /** Gets the id */
  val field_id = new TouchField("id",TString.typName)

  /** Gets the name of the user */
  val field_name = new TouchField("name",TString.typName)

  /** Gets the url of the user picture where original is the unmodified user picture, square is 50x50, small has 50px
    * width, normal has 100px width, large has roughly 200px width */
  val field_picture_address = new TouchField("picture address",TString.typName)

  /** Gets the user picture where original is the unmodified user picture, square is 50x50, small has 50px width,
    * normal has 100px width, large has roughly 200px width */
  val field_picture = new TouchField("picture",TPicture.typName)

  val typName = "User"
  val typ = new TouchType(typName,isSingleton = false,fields = List(field_about, field_has_picture, field_id,
    field_name, field_picture_address, field_picture))

}

class TUser extends AAny {

  def getTyp = TUser.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet],
                                               returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
