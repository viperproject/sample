
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Sprite Sheet
 *
 * A sprite sheet which packs multiple frames in a single picture
 *
 * @author Lucas Brutschy
 */

object TSprite_Sheet extends AAny {

  /** Gets the picture associated to this sprite sheet. */
  lazy val field_picture = new ApiField("picture", TPicture.typeName)

  val typeName = TypeName("Sprite Sheet")

  override def possibleFields = super.possibleFields ++ List(field_picture)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Defines an animation as a custom sequence of frames. */
    case "add animation" =>
      val List(name, frames, duration, loops, yoyo) = parameters // String,String_Collection,Number,Number,Boolean
      Skip

    /** Defines a new frame in the sprite sheet */
    case "add frame" =>
      val List(name, x, y, width, height, rotated) = parameters // String,Number,Number,Number,Number,Boolean
      Skip

    /** Defines an animation as a continuous sequence of frames. The frame index starts at `1`. */
    case "add grid animation" =>
      val List(name, start, count, duration, loops, yoyo) = parameters // String,Number,Number,Number,Number,Boolean
      Skip

    /** Creates a new sprite displaying the given frame. */
    case "create sprite" =>
      val List(frame) = parameters // String
      Top[S](TSprite)

    /** Sets the frames as a rectangular grid. The tiles are numbered from top, left to bottom right starting at 0. */
    case "set frame grid" =>
      val List(rows, columns, frame_width, frame_height, margin_left, margin_top, spacing) = parameters // Number,Number,Number,Number,Number,Number,Number
      Skip

    /** Sets the current frame displayed by sprite */
    case "set frame" =>
      val List(sprite, frame) = parameters // Sprite,String
      Skip

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
