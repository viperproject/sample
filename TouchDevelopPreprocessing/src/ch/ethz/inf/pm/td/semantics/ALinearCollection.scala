/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, ProgramPoint}
import ch.ethz.inf.pm.td.analysis.{TouchAnalysisParameters, RichExpressionSet}
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._
import ch.ethz.inf.pm.td.domain.TouchState


/**
 * This class represents collections that
 * have linear integer keys.
 **/
trait ALinearCollection extends ACollection {

  override def member_at_index = super.member_at_index.copy(semantics = new ApiMemberSemantics {
    override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
      val List(index) = parameters // Key_Type
      // Check disabled -- ALWAYS FALSE ALARM!
      //CheckInRangeInclusive[S](index,0,(CollectionSize[S](this0)-NumericalAnalysisConstants.epsilon),method,"index")
      Return[S](At[S](this0, index))
    }
  })

  override def At[S <: State[S]](collection: RichExpressionSet, key: RichExpressionSet)(implicit state: S, pp: ProgramPoint): RichExpressionSet = {
    val res = if (TouchAnalysisParameters.get.collectionsSummarizeLinearElements) {
      AllValues[S](collection)
    } else {
      super.At[S](collection, key)
    }
    res
  }

  override def declarations: Map[String, ApiMember] = super.declarations ++ Map(
    "at" -> member_at,
    "rand" -> member_rand,
    "random" -> member_random
  )

  /** Sometimes used: Gets the picture at position 'index'; invalid if index is out of bounds */
  def member_at = ApiMember(
    name = "at",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = valueType,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        val List(index) = parameters // Key_Type

        if (index.typ != TNumber)
          throw new SemanticException("This is not a linear collection " + this0.toString)

        val newState = If[S](IndexInRange[S](this0, index), Then = { thenState =>
          Return[S](At[S](this0, index))(thenState, pp)
        }, Else = { elseState =>
          Return[S](Invalid(this0.typ.asInstanceOf[ACollection].valueType, "collection access may be out of range"))(elseState, pp)
        })
        newState
      }
    }
  )

  def IndexInRange[S <: State[S]](collection: RichExpressionSet, index: RichExpressionSet)(implicit state: S, pp: ProgramPoint): RichExpressionSet = {
    index >= 0 && index < Count[S](collection)
  }

  /** Never used: Renamed to 'random' */
  def member_rand = ApiMember(
    name = "rand",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = valueType,
    semantics = RandomSemantics
  )

  /** Sometimes used: Gets a random picture; invalid if collection is empty */
  def member_random = ApiMember(
    name = "random",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = valueType,
    semantics = RandomSemantics
  )

  def collectionContainsValue[S <: State[S]](collection: RichExpressionSet, value: RichExpressionSet)(implicit state: S, pp: ProgramPoint): RichExpressionSet = {
    // Improve precision: Always true if collection size must be empty
    if (Assume[S](Count[S](collection) > 0).isBottom) {
      return False
    }
    val x = If[S](AllValues[S](collection) equal value, { thenState: S =>
      Return[S](True)
    }, { els: S =>
      Return[S](False)
    }).expr
    x
  }

  def InvalidateKeys[S <: State[S]](collection: RichExpressionSet)(implicit state: S, pp: ProgramPoint): S = {
    Assign[S](AllKeys[S](collection),0 ndToIncl (Count[S](collection) - 1))
  }

  def Add[S <: State[S]](this0: RichExpressionSet, value: RichExpressionSet)(implicit state: S, pp: ProgramPoint): S = {
    var curState = state
    curState = Insert[S](this0, Count[S](this0), value)(curState, pp)
    curState = IncreaseLength[S](this0)(curState, pp)
    curState
  }

  /**
   * This overrides the definition of collection insert in general collections.
   * Generally, there is no need to represent the entries of a linear collection separately.
   * Instead, we always use the same pp for all collections.
   */
  override def Insert[S <: State[S]](collection: RichExpressionSet, index: RichExpressionSet, right: RichExpressionSet)(implicit state: S, pp: ProgramPoint): S = {
    var curState = state
    val idPP = if (TouchAnalysisParameters.get.collectionsSummarizeLinearElements) DummyProgramPoint else pp
    curState = New[S](entryType, initials = Map(
      entryType.field_key -> index,
      entryType.field_value -> right
    ))(curState, idPP)
    curState = AssignField[S](collection, field_entry, curState.expr)(curState, idPP)
    curState
  }

  object RandomSemantics extends ApiMemberSemantics {
    override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
      If[S](Count[S](this0) > 0, Then = {
        Return[S](AllValues[S](this0))(_, pp)
      }, Else = {
        Return[S](Invalid(this0.typ.asInstanceOf[ACollection].valueType, "collection may be empty"))(_, pp)
      })
    }
  }

  object InvalidateKeysSemantics extends ApiMemberSemantics {
    override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
      InvalidateKeys(this0)
    }
  }

}
