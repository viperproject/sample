
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.TouchField
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of User
 *
 * A user account
 *
 * @author Lucas Brutschy
 */ 

object TUser extends AAny {

  /** Gets the about-me text of the user */
  lazy val field_about = new TouchField("about",TString.typeName)

  /** Indicates if the user has a picture */
  lazy val field_has_picture = new TouchField("has picture",TBoolean.typeName)

  /** Gets the id */
  lazy val field_id = new TouchField("id",TString.typeName)

  /** Gets the name of the user */
  lazy val field_name = new TouchField("name",TString.typeName)

  /** Gets the url of the user picture where original is the unmodified user picture, square is 50x50, small has 50px
    * width, normal has 100px width, large has roughly 200px width */
  lazy val field_picture_address = new TouchField("picture address",TString.typeName)

  /** Gets the user picture where original is the unmodified user picture, square is 50x50, small has 50px width,
    * normal has 100px width, large has roughly 200px width */
  lazy val field_picture = new TouchField("picture",TPicture.typeName)

  val typeName = TypeName("User")

  override def possibleFields = super.possibleFields ++ List(field_about, field_has_picture, field_id,
    field_name, field_picture_address, field_picture)


  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet],
                                               returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
