package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{SemanticException, ExpressionSet, State}
import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._
import ch.ethz.inf.pm.td.analysis.interpreter._
import ch.ethz.inf.pm.td.analysis.interpreter.RefV
import ch.ethz.inf.pm.td.analysis.interpreter.NumberV
import ch.ethz.inf.pm.td.compiler.TouchCollection
import scala.Some

/**
* This class represents collections that
* have linear integer keys.
**/
abstract class ALinearCollection extends ACollection {
  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S) = method match {
    case "at" =>
      val List(index) = parameters // Key_Type

      if (index.getType().name != TNumber.typName)
        throw new SemanticException("This is not a linear collection " + this0.toString)

      val newState = If[S](CollectionIndexInRange[S](this0, index), Then={
        Return[S](CollectionAt[S](this0, index))(_, pp)
      }, Else={
        Return[S](Invalid(this0.getType().asInstanceOf[TouchCollection].valueType))(_, pp)
      })
      newState

    /** Gets the i-th element */
    case "at index" =>
      val List(index) = parameters // Key_Type
      // Check disabled -- ALWAYS FALSE ALARM!
      //CheckInRangeInclusive[S](index,0,(CollectionSize[S](this0)-NumericalAnalysisConstants.epsilon),method,"index")
      Return[S](CollectionAt[S](this0,index))


    /** Get random element */
    case "random" =>
      val valType = this0.getType().asInstanceOf[TouchCollection].valueType

      If[S](CollectionSize[S](this0) > 0, Then= { s =>
        val s2 = NonDetReturn[S](valType, CollectionSummary[S](this0))(s, pp)
        ReturnTemp[S](s2, pp)
      }, Else={
        ReturnTemp[S](Invalid(valType))(_, pp)
      })

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)
  }

  override def concreteSemantics(this0: TouchValue,
                                 method: String,
                                 params: List[TouchValue],
                                 interpreter: ConcreteInterpreter,
                                 pp: ProgramPoint): TouchValue = method match {

    case "at index" =>
      val state = interpreter.state
      (this0, params) match {
        case (collRef: RefV, List(index: NumberV)) =>
          interpreter.assertE(index.v == index.v.floor && !index.v.isInfinite)(pp)
          val collObj = state.getCollection(collRef)
          collObj.entries(index)
      }

    case "random" =>
      val state = interpreter.state
      val collTyp = this0.typ.asInstanceOf[TouchCollection]
      this0 match {
        case collRef: RefV =>
          val collObj = state.getCollection(collRef)
          if (collObj.entries.isEmpty) return InvalidV(collTyp.valueType)

          interpreter.nonDetInputAt(pp).getOrElse({
              val entries = collObj.entries
              val randIdx = state.random.nextInt(collObj.entries.size)
              val keys = entries.keys.toList
              collObj.entries(keys(randIdx))
          })
      }

    case _ => super.concreteSemantics(this0, method, params, interpreter, pp)
  }
}
