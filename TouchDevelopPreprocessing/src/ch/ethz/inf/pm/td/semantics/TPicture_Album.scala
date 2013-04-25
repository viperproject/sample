
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Picture Album
 *
 * A picture album
 *
 * @author Lucas Brutschy
 */ 

object TPicture_Album {

  /** Gets the children albums */
  val field_albums = new TouchField("albums",TPicture_Albums.typ)

  /** Gets the name of the album */
  val field_name = new TouchField("name",TString.typ)

  /** Gets the pictures */
  val field_pictures = new TouchField("pictures",TPictures.typ)

  val typName = "Picture Album"
  val typ = new TouchType(typName,isSingleton = false, fields = List(field_albums, field_name, field_pictures))

}

class TPicture_Album extends AAny {

  def getTyp = TPicture_Album.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
