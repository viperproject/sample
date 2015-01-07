package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, SemanticException, State}
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, ProgramPoint}
import ch.ethz.inf.pm.td.analysis.{RichExpression}
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._
import ch.ethz.inf.pm.td.domain.TouchState


/**
 * This class represents collections that
 * have linear integer keys.
 **/
trait ALinearCollection extends ACollection {


  /** Sometimes used: Gets the picture at position 'index'; invalid if index is out of bounds */
  def member_at = ApiMember(
    name = "at",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = valueType,
    semantics = DefaultSemantics
  )

  /** Never used: Renamed to 'random' */
  def member_rand = ApiMember(
    name = "rand",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = valueType,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets a random picture; invalid if collection is empty */
  def member_random = ApiMember(
    name = "random",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = valueType,
    semantics = DefaultSemantics
  )

  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "at" -> member_at,
    "rand" -> member_rand,
    "random" -> member_random
  )

  def collectionContainsValue[S <: State[S]](collection: RichExpression, value: RichExpression)(implicit state: S, pp: ProgramPoint): RichExpression = {
    If[S](collectionAllValues[S](collection) equal value, { then: S =>
      Return[S](True)
    }, { els: S =>
      Return[S](False)
    }).expr
  }

  def collectionInvalidateKeys[S <: State[S]](collection: RichExpression)(implicit state: S, pp: ProgramPoint): S = {
    Assign[S](collectionAllKeys[S](collection),0 ndTo collectionSize[S](collection) - 1)
  }

  def collectionIndexInRange[S <: State[S]](collection: RichExpression, index: RichExpression)(implicit state: S, pp: ProgramPoint): RichExpression = {
    index >= 0 && index < collectionSize[S](collection)
  }

  /**
   * This overrides the definition of collection insert in general collections.
   * Generally, there is no need to represent the entries of a linear collection separately.
   * Instead, we always use the same pp for all collections.
   */
  override def collectionInsert[S <: State[S]](collection: RichExpression, index: RichExpression, right: RichExpression)(implicit state: S, pp: ProgramPoint): S = {
    var curState = state
    curState = New[S](entryType, initials = Map(
      entryType.field_key -> index,
      entryType.field_value -> right
    ))(curState, DummyProgramPoint)
    curState = AssignField[S](collection, field_entry, curState.expr.add(Field[S](collection, field_entry)))(curState, pp)
    curState
  }

  object InvalidateKeysSemantics extends ApiMemberSemantics {
    override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
      collectionInvalidateKeys(this0)
    }
  }

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S) = method match {
    case "at" =>
      val List(index) = parameters // Key_Type

      if (index.getType() != TNumber)
        throw new SemanticException("This is not a linear collection " + this0.toString)

      val newState = If[S](collectionIndexInRange[S](this0, index), Then = {
        Return[S](collectionAt[S](this0, index))(_, pp)
      }, Else = {
        Return[S](Invalid(this0.getType().asInstanceOf[ACollection].valueType, "collection access may be out of range"))(_, pp)
      })
      newState

    /** Gets the i-th element */
    case "at index" =>
      val List(index) = parameters // Key_Type
      // Check disabled -- ALWAYS FALSE ALARM!
      //CheckInRangeInclusive[S](index,0,(CollectionSize[S](this0)-NumericalAnalysisConstants.epsilon),method,"index")
      Return[S](collectionAt[S](this0, index))

    /** Get random element */
    case "random" =>
      If[S](collectionSize[S](this0) > 0, Then = {
        Return[S](collectionAllValues[S](this0))(_, pp)
      }, Else = {
        Return[S](Invalid(this0.getType().asInstanceOf[ACollection].valueType, "collection may be empty"))(_, pp)
      })

    /** Get random element (old name) */
    case "rand" =>
      If[S](collectionSize[S](this0) > 0, Then = {
        Return[S](collectionAllValues[S](this0))(_, pp)
      }, Else = {
        Return[S](Invalid(this0.getType().asInstanceOf[ACollection].valueType, "collection may be empty"))(_, pp)
      })

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)
  }
}
