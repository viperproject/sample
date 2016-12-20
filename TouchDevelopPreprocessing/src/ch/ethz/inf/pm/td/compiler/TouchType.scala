/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.compiler

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State, TypeMap}
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
import ch.ethz.inf.pm.sample.{SystemParameters, oorepresentation}
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._
import ch.ethz.inf.pm.td.analysis.{RichNativeSemantics, TouchAnalysisParameters}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._


trait TouchType extends Named with Type {

  lazy val name:String = typeName.toString
  def typeName:TypeName

  def isSingleton: Boolean
  def isImmutable: Boolean

  def factory(): TopTouchType.type = top()

  def top() = TopTouchType

  def glb(other: oorepresentation.Type): oorepresentation.Type = {
    if (other == null) return this
    val other_ = other.asInstanceOf[TouchType]
    if (isBottom || other_.isBottom) return bottom()
    if (isTop) return other_
    if (other_.isTop) return this
    if (!equals(other_)) bottom()
    else this
  }

  def bottom() = BottomTouchType

  def isTop: Boolean = this == TopTouchType

  def isBottom: Boolean = this == BottomTouchType

  def widening(other: Type): Type = lub(other)

  def lub(other: oorepresentation.Type): oorepresentation.Type = {
    if (other == null) return this
    val other_ = other.asInstanceOf[TouchType]
    if (isTop || other_.isTop) return top()
    if (isBottom) return other_
    if (other_.isBottom) return this
    if (!equals(other_)) top()
    else this
  }

  def lessEqual(other: Type): Boolean = other == this || this.isBottom || other == top()

  def isObject: Boolean = !isNumericalType && !isStringType

  def isNumericalType: Boolean = name == "Number" || name == "Boolean"

  def isStringType: Boolean = name == "String"

  def isBooleanType: Boolean = name == "Boolean"

  def isFloatingPointType: Boolean = name == "Number" || name == "Boolean" // TODO: Booleans should not be floating points

  def isStatic: Boolean = isSingleton

}

case object TopTouchType extends AAny {

  val typeName = TypeName("Top")
  override def isTop: Boolean = true

}

case object BottomTouchType extends AAny {

  val typeName = TypeName("Bottom")
  override def isBottom: Boolean = true

}

case class ApiParam(typ:AAny, isMutated:Boolean = false)

case class ApiMember(
                      name:String,
                      paramTypes:List[ApiParam],
                      thisType:ApiParam,
                      returnType:AAny,
                      semantics:ApiMemberSemantics,
    pausesInterpreter: Boolean = false,
    isAsync: Boolean = false,
    infixPriority: Double = 0.0,
                      runOnInvalid:Boolean = false
                      ) {

}

trait ApiMemberSemantics {

  def forwardSemantics[S <: State[S]](this0: ExpressionSet, method:ApiMember, parameters: List[ExpressionSet])
                                     (implicit pp: ProgramPoint, state: S): S

}

object TopSemantics extends ApiMemberSemantics {

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method:ApiMember, parameters: List[ExpressionSet])
                                              (implicit pp: ProgramPoint, state: S): S = {
    Top[S](method.returnType)
  }

}

object TopWithInvalidSemantics extends ApiMemberSemantics {

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method:ApiMember, parameters: List[ExpressionSet])
                                              (implicit pp: ProgramPoint, state: S): S = {
    TopWithInvalid[S](method.returnType, "Return value may be invalid")
  }

}

object SkipSemantics extends ApiMemberSemantics {

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method:ApiMember, parameters: List[ExpressionSet])
                                              (implicit pp: ProgramPoint, state: S): S = {
    if (SystemParameters.DEBUG) assert(method.returnType == TNothing)
    Skip[S]
  }

}


object IdentitySemantics extends ApiMemberSemantics {

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method:ApiMember, parameters: List[ExpressionSet])
                                              (implicit pp: ProgramPoint, state: S): S = {
    if (SystemParameters.DEBUG) assert(method.returnType == this0.typ)
    Return[S](this0)
  }

}




object StopSemantics extends ApiMemberSemantics {

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method:ApiMember, parameters: List[ExpressionSet])
                                              (implicit pp: ProgramPoint, state: S): S = {
    if (SystemParameters.DEBUG) assert(method.returnType == TNothing)
    state.bottom()
  }

}

object ExitSemantics extends ApiMemberSemantics {
  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
    Exit[S]
  }
}

trait AbstractInvalidSemantics extends ApiMemberSemantics {

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method:ApiMember, parameters: List[ExpressionSet])
                                              (implicit pp: ProgramPoint, state: S): S = {
    Return[S](Invalid(method.returnType, "Return value may be invalid"))
  }

}

object InvalidSemantics extends ApiMemberSemantics with AbstractInvalidSemantics

object ScheduleEventHandlerSemantics extends ApiMemberSemantics {

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember,
                                               parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {

    for (p <- parameters) {
      p.typ match {
        case a:AAction =>
          //a.EnableHandler[S](p)(pp,state)
        case _ =>
          ()
      }
    }

    state
  }

}

/**
 * Sound semantics
 */
object DefaultSemantics extends ApiMemberSemantics {

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method:ApiMember, parameters: List[ExpressionSet])
                                              (implicit pp: ProgramPoint, state: S): S = {
    if (SystemParameters.DEBUG) assert (parameters.length == method.paramTypes.length)

    var curState = state

    // Is this just a getter of a field?
    if (!method.thisType.isMutated && parameters.isEmpty) { // FIXME: Faster access
      method.thisType.typ.representedTouchFields.find(_.getName == method.name) match {
        case Some(x) =>
          return Return[S](Field[S](this0,x))(curState,pp)
        case _ => ()
      }
      method.thisType.typ.mutedFields.find(_.getName == method.name) match {
        case Some(x) =>
          return Top[S](x.typ)(curState,pp) // Not sound by design
        case _ => ()
      }
    }

    // Is this just a setter for a field?
    if (method.thisType.isMutated && parameters.size == 1) {
      val Setter = """set (.*)""".r
      method.name match {
        case Setter(x) if method.thisType.typ.representedFields.exists(_.getName == x) => // FIXME: Faster access
          return AssignField[S](this0,x,parameters.head)(curState,pp)
        case Setter(x) if method.thisType.typ.mutedFields.exists(_.getName == x) => // FIXME: Faster access
          return Skip[S]
        case _  => ()
      }
    }

    RichNativeSemantics.Dummy[S](this0,method.name)(state,pp)

    if (TouchAnalysisParameters.get.defaultToUnsound) {

      // Return top with invalid
      curState = Top[S](method.returnType)(curState,pp)

    } else {

      // Set everything we mutate to top with invalid
      if (method.thisType.isMutated) {
        curState = SetToTopWithInvalid[S](this0, "Potentially invalidated by " + method.name)(curState, pp)
      }
      for ((paramExpr, paramTyp) <- parameters.zip(method.paramTypes)) {
        if (paramTyp.isMutated) {
          curState = SetToTopWithInvalid[S](paramExpr, "Potentially invalidated by " + method.name)(curState, pp)
        }
      }

      // Return top with invalid
      curState = TopWithInvalid[S](method.returnType, "returned type may be invalid (default semantics)")(curState, pp)

    }

    curState
  }

}


/**
 * Sound semantics
 */
object NewSemantics extends ApiMemberSemantics {

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method:ApiMember, parameters: List[ExpressionSet])
                                              (implicit pp: ProgramPoint, state: S): S = {
    New[S](method.returnType)
  }

}

/**
 * Defines a semantics that returns any valid value of the return type.
 * The semantics does not define any side effects.
 */
object ValidPureSemantics extends ApiMemberSemantics {

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method:ApiMember, parameters: List[ExpressionSet])
                                              (implicit pp: ProgramPoint, state: S): S = {

    if (SystemParameters.DEBUG) {
      assert(!method.thisType.isMutated, "valid pure method "+method+" should not mutate its receiver: "+this0)
      assert(method.paramTypes.forall(!_.isMutated), "valid pure method "+method+" should not mutate its parameters: "+parameters)
    }

    Top[S](method.returnType)

  }

}

object TouchTypeMap extends TypeMap {
  override val Int: Type = TNumber
  override val Float: Type = TNumber
  override val String: Type = TString
  override val Boolean: Type = TBoolean
  override val Bottom: Type = BottomTouchType
  override val Top: Type = TopTouchType
}