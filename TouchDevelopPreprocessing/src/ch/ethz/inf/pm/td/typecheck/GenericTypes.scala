package ch.ethz.inf.pm.td.typecheck

import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics.{TString, TNumber, TNothing, TBoolean}


/**
 *
 * Lucas Brutschy
 * Date: 8/21/12
 * Time: 10:34 AM
 *
 */
object GenericTypes {

  def gAlsoSingletons(thisName: TypeName): List[Member] = List(
    Member("∥", List(TString.typeName), TString.typeName),
    Member("equals", List(thisName), TBoolean.typeName)
  )

  def gAny(thisName: TypeName): List[Member] = gAlsoSingletons(thisName) ::: List(
    Member("is invalid", TBoolean.typeName),
    Member("post to wall", TNothing.typeName),
    Member("◈get", thisName) /* Get the current value of the reference */ ,
    Member("◈confirmed", TBoolean.typeName) /* Check if reference has been written to the storage/server */ ,
    Member("◈set", List(thisName), TNothing.typeName) /* Set the value of the reference */ ,
    Member("◈clear", TNothing.typeName) /* Set reference to invalid */ ,
    Member("◈ref", "Ref " + thisName) /* Retrive the reference itself (useful on globals and fields) */ ,
    Member("◈add", List(TNumber.typeName), TNothing.typeName) /* Add specified value to given reference */ ,
    Member("◈test and set", List(thisName), TNothing.typeName) /* Set reference to `v` if it's currently non-empty */
  )

  def gIndex(thisName: TypeName, keyTypes: List[TypeName], valueType: TypeName): List[Member] =
    if (keyTypes.size > 0) gAny(thisName) ::: List(
      Member("at", keyTypes, valueType),
      Member("at index", List(TNumber.typeName), valueType),
      Member("count", TNumber.typeName),
      Member("clear", TNothing.typeName),
      Member("copy", thisName)
    )
    else gAny(thisName) ::: List(
      Member("singleton", valueType)
    )

  def gIndexMember(thisName: TypeName, fieldsAndKeys: List[Member]): List[Member] = gAny(thisName) ::: fieldsAndKeys ::: List(
    Member("clear fields", TNothing.typeName)
  )

  def gCollection(thisName: TypeName, typ: TypeName): List[Member] = gAny(thisName) ::: List(
    Member("at", List(TNumber.typeName), typ),
    Member("at index", List(TNumber.typeName), typ),
    Member("copy", List(), thisName),
    Member("count", TNumber.typeName),
    Member("random", typ) // Gets a random item; invalid if collection is empty
  )

  def gRow(thisName: TypeName, fields: List[Member]): List[Member] = gAny(thisName) ::: fields ::: List(
    Member("delete row", List(), TNothing.typeName),
    Member("confirmed", List(), TBoolean.typeName)
  )

  def gObject(thisName: TypeName, fields: List[Member]): List[Member] = gAny(thisName) ::: fields ::: List(
    Member("clear fields", List(), TNothing.typeName)
  )

  def gTable(thisName: TypeName, typ: TypeName): List[Member] = gCollection(thisName, typ) ::: List(
    Member("add row", List(), typ),
    Member("row at", List(TNumber.typeName), typ),
    Member("clear", List(), TNothing.typeName),
    Member("invalid row", List(), typ)
  )

  def gMutableCollection(thisName: TypeName, typ: TypeName): List[Member] = gCollection(thisName, typ) ::: List(
    Member("add", List(typ), TNothing.typeName),
    Member("add many", List(thisName), TNothing.typeName),
    Member("clear", TNothing.typeName),
    Member("contains", List(typ), TBoolean.typeName), //Indicates if the collection contains the item
    Member("index of", List(typ, TNumber.typeName), TNumber.typeName),
    Member("insert at", List(TNumber.typeName, typ), TNothing.typeName),
    Member("random", typ),
    Member("remove", List(typ), TBoolean.typeName),
    Member("remove at", List(TNumber.typeName), TNothing.typeName),
    Member("reverse", TNothing.typeName),
    Member("@reverse", TNothing.typeName), // old name?
    Member("set at", List(TNumber.typeName, typ), TNothing.typeName)
  )

}
