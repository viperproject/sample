package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{SetDomain, ExpressionSet, Identifier, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{TouchDevelopEntryStateBuilder, RichExpression, TouchField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.{TouchCompiler, TypeList, TouchType}
import ch.ethz.inf.pm.td.domain.{FieldIdentifier, HeapIdentifier, TouchState}
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Represents a collection (this class contains common read operations. Extend AMutable_Collections to get write ops)
 */
trait ACollection extends AAny {

  def keyTypeName:TypeName
  def valueTypeName:TypeName

  lazy val entryType = GEntry(keyTypeName,valueTypeName)

  lazy val keyType =   SystemParameters.compiler.asInstanceOf[TouchCompiler].getType(keyTypeName)
  lazy val valueType = SystemParameters.compiler.asInstanceOf[TouchCompiler].getType(valueTypeName)

  lazy val field_count = TouchField("count",TNumber.typeName)
  lazy val field_entry = TouchField("entries",entryType.typeName)

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
    val entryType = GEntry(keyTypeName,valueTypeName)
    curState = New[S](entryType,initials = Map(
      entryType.field_key -> index,
      entryType.field_value -> right
    ))(curState,pp)
    curState = AssignField[S](collection,field_entry,curState.expr.add(Field[S](collection,field_entry)))(curState,pp)
    curState
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
      Invalid(GEntry(keyTypeName,valueTypeName),"collection cleared"))(curState,pp)
    curState
  }

  def collectionAt[S <: State[S]](collection: RichExpression, key: RichExpression)(implicit state: S, pp: ProgramPoint): RichExpression = {
    state match {
      case tS: TouchState.Default[TouchDevelopEntryStateBuilder.SemanticDomainType] =>
        new ExpressionSet(entryType.field_value.typ,SetDomain.Default(
          tS.getFieldValueWhere(collection,field_entry.getField.get,field_entry.typ,
          {
            (x:HeapIdentifier,s:TouchState.Default[TouchDevelopEntryStateBuilder.SemanticDomainType]) =>
              !s.assume(FieldIdentifier(x,entryType.field_key.getField.get,entryType.field_key.typ) equal key).isBottom
          }
          )._1.map(FieldIdentifier(_,entryType.field_value.getField.get,entryType.field_value.typ))))
      case _ => collectionAllValues[S](collection)
    }
  }

  def collectionContainsKey[S <: State[S]](collection: RichExpression, key: RichExpression)(implicit state: S, pp: ProgramPoint): RichExpression = {
    state match {
      case tS: TouchState.Default[TouchDevelopEntryStateBuilder.SemanticDomainType] =>

        val (mayMatching,mustMatching) = tS.getFieldValueWhere(collection,field_entry.getField.get,field_entry.typ,
          {
            (x:HeapIdentifier,s:TouchState.Default[TouchDevelopEntryStateBuilder.SemanticDomainType]) =>
              !s.assume(FieldIdentifier(x,entryType.field_key.getField.get,entryType.field_key.typ) equal key).isBottom
          }
          )

        if (mayMatching.nonEmpty) True
        else if (mustMatching.isEmpty) False
        else True or False

      case _ => True or False
    }
  }

  def collectionRemoveFirst[S <: State[S]](collection: RichExpression, value: RichExpression)(implicit state: S, pp: ProgramPoint) = {
    state // TODO
  }

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Creates a copy of the given collection. AUXILIARY FUNCTION FOR FOREACH LOOPS */
    case "copy" =>
      Clone[S](this0)

    /** [**dbg**] Exports a JSON representation of the contents. */
    case "to json" =>
      Top[S](TJson_Object)

    /** [**dbg**] Imports a JSON representation of the contents. */
    case "from json" =>
      val List(jobj) = parameters // Json_Object UNSOUND
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
