
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.ApiField
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of Picture Album
 *
 * A picture album
 *
 * @author Lucas Brutschy
 */ 

object TPicture_Album extends AAny {

  /** Gets the children albums */
  lazy val field_albums = new ApiField("albums",TPicture_Albums.typeName)

  /** Gets the name of the album */
  lazy val field_name = new ApiField("name",TString.typeName)

  /** Gets the pictures */
  lazy val field_pictures = new ApiField("pictures",TPictures.typeName)

  lazy val typeName = TypeName("Picture Album")

  override def possibleFields = super.possibleFields ++ List(field_albums, field_name, field_pictures)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
