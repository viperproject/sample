package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, ProgramPoint}
import ch.ethz.inf.pm.td.analysis._
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.domain.{HeapIdentifier, TouchState}
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Represents a collection (this class contains common read operations. Extend AMutable_Collections to get write ops)
 */
trait ACollection extends AAny {

  def keyType:AAny
  def valueType:AAny

  /** Exports a JSON representation of the contents. */
  def member_to_json = new ApiMember(
    name = "to json",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TJson_Object,
    semantics = TopSemantics
  )

  /** Gets the number of elements */
  def member_count = new ApiMember(
    name = "count",
    paramTypes = List(),
    thisType = ApiParam(this),
    TNumber,
    DefaultSemantics
  )

  /** Imports a JSON representation of the contents. */
  def member_from_json = new ApiMember(
    name = "from json",
    paramTypes = List(ApiParam(TJson_Object)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    DefaultSemantics
  )

  /** Never used: Clears the values from the map */
  def member_copy = ApiMember(
    name = "copy",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = this,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        if (TouchAnalysisParameters.assumeCollectionsNotModifiedDuringIteration) {
          Return[S](this0)
        } else {
          Clone[S](this0)
        }
      }
    }
  )

  /** Never used: Clears the values from the map */
  def member_at_index = ApiMember(
    name = "at index",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = valueType,
    semantics = DefaultSemantics
  )

  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "to json" -> member_to_json,
    "count" -> member_count,
    "from json" -> member_from_json,
    "copy" -> member_copy,
    "at index" -> member_at_index
  )

  lazy val entryType = GEntry(keyType,valueType)

  lazy val field_count = ApiField("count",TNumber)
  lazy val field_entry = ApiField("entries",entryType)

  override def isSingleton: Boolean = false

  override def possibleFields: Set[Identifier] =
    super.possibleFields ++ Set(field_entry,field_count)

  def collectionAllKeys[S <: State[S]](collection: RichExpression)(implicit state: S, pp: ProgramPoint): RichExpression = {
    Field[S](Field[S](collection,field_entry),field_entry.typ.asInstanceOf[GEntry].field_key)
  }

  def collectionAllValues[S <: State[S]](collection: RichExpression)(implicit state: S, pp: ProgramPoint): RichExpression = {
    Field[S](Field[S](collection,field_entry),field_entry.typ.asInstanceOf[GEntry].field_value)
  }

  def collectionSize[S <: State[S]](collection: RichExpression)(implicit state: S, pp: ProgramPoint): RichExpression = {
    Field[S](collection,field_count)
  }

  def collectionSetSize[S <: State[S]](collection: RichExpression, right: RichExpression)(implicit state: S, pp: ProgramPoint): S = {
    AssignField[S](collection,field_count,right)
  }

  def collectionInsert[S <: State[S]](collection: RichExpression, index: RichExpression, right: RichExpression)(implicit state: S, pp: ProgramPoint): S = {
    var curState = state
    val idPP = if (TouchAnalysisParameters.collectionsSummarizeLinearElements) DummyProgramPoint else pp
    curState = New[S](entryType, initials = Map(
      entryType.field_key -> index,
      entryType.field_value -> right
    ))(curState, idPP)
    curState = AssignField[S](collection, field_entry, curState.expr.add(Field[S](collection, field_entry)))(curState, idPP)
    curState
  }

  def collectionUpdate[S <: State[S]](collection: RichExpression, key: RichExpression, value: RichExpression)(implicit state: S, pp: ProgramPoint): S = {
    val newState = collectionRemoveAt[S](collection, key)(state, pp)
    collectionInsert[S](collection, key, value)(newState, pp)
  }

  def collectionIncreaseLength[S <: State[S]](collection: RichExpression)(implicit state: S, pp: ProgramPoint): S = {
    AssignField[S](collection, field_count, collectionSize[S](collection) + 1)
  }

  def collectionDecreaseLength[S <: State[S]](collection: RichExpression)(implicit state: S, pp: ProgramPoint): S = {
    AssignField[S](collection, field_count, collectionSize[S](collection) - 1)
  }

  def collectionClear[S <: State[S]](collection: RichExpression)(implicit state: S, pp: ProgramPoint): S = {
    var curState = state
    curState = collectionSetSize[S](collection, 0)(curState, pp)
    curState = AssignField[S](collection,field_entry,
      Invalid(entryType,"collection cleared"))(curState,pp)
    curState
  }

  def collectionAt[S <: State[S]](collection: RichExpression, key: RichExpression)(implicit state: S, pp: ProgramPoint): RichExpression = {
    if (TouchAnalysisParameters.collectionsSummarizeElements) {
      collectionAllValues[S](collection)
    } else {
      Lattice.bigLub[ExpressionSet](
        state.getFieldValueWhere(collection, field_entry.getField.get, field_entry.typ, {
          (x: Identifier, s: S) =>
            !s.assume(Field[S](x, entryType.field_key)(s, pp) equal key).isBottom
        }
        )._1.map({
          x: Identifier => toExpressionSet(Field[S](x, entryType.field_value)(state, pp))
        })
      )
    }
  }

  def collectionRemoveAt[S <: State[S]](collection: RichExpression, key: RichExpression)(implicit state: S, pp: ProgramPoint): S = {
    // find all nodes which must match the key, and remove them
    val (_,nodesToBeRemoved) =
      state.getFieldValueWhere(collection,field_entry.getField.get,field_entry.typ,
      {
        (x:Identifier,s:S) =>
          s.assume(Field[S](x,entryType.field_key)(s,pp) unequal key).isBottom
      })
    val r = new Replacement(isPureRemoving = true)
    for (n <- nodesToBeRemoved) {
      r.value += (Set[Identifier](n) -> Set.empty[Identifier])
    }
    val noEqualKey = Field[S](Field[S](collection,field_entry),entryType.field_key) unequal key

    state.merge(r).assume(noEqualKey)
  }

  def collectionContainsKey[S <: State[S]](collection: RichExpression, key: RichExpression)(implicit state: S, pp: ProgramPoint): RichExpression = {
    val (mayMatching,mustMatching) = state.getFieldValueWhere(collection,field_entry.getField.get,field_entry.typ,
      {
        (x:Identifier,s:S) =>
          !s.assume(Field[S](x,entryType.field_key)(s,pp) equal key).isBottom
      }
    )

    if (mayMatching.nonEmpty) True
    else if (mustMatching.isEmpty) False
    else True or False
  }

  def collectionRemoveFirst[S <: State[S]](collection: RichExpression, value: RichExpression)(implicit state: S, pp: ProgramPoint) = {
    state // TODO
  }

}
