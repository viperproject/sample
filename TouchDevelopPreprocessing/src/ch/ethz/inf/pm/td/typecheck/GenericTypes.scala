package ch.ethz.inf.pm.td.typecheck


/**
 *
 * Lucas Brutschy
 * Date: 8/21/12
 * Time: 10:34 AM
 *
 */
object GenericTypes {

  def gAlsoSingletons(thisName: String): List[Member] = List(
    Member("∥", List("String"), "String"),
    Member("equals", List(thisName), "Boolean")
  )

  def gAny(thisName: String): List[Member] = gAlsoSingletons(thisName) ::: List(
    Member("is invalid", "Boolean"),
    Member("post to wall", "Nothing"),
    Member("◈get", thisName) /* Get the current value of the reference */ ,
    Member("◈confirmed", "Boolean") /* Check if reference has been written to the storage/server */ ,
    Member("◈set", List(thisName), "Nothing") /* Set the value of the reference */ ,
    Member("◈clear", "Nothing") /* Set reference to invalid */ ,
    Member("◈ref", "Ref " + thisName) /* Retrive the reference itself (useful on globals and fields) */ ,
    Member("◈add", List("Number"), "Nothing") /* Add specified value to given reference */ ,
    Member("◈test and set", List(thisName), "Nothing") /* Set reference to `v` if it's currently non-empty */
  )

  def gIndex(thisName: String, keyTypes: List[String], valueType: String): List[Member] =
    if (keyTypes.size > 0) gAny(thisName) ::: List(
      Member("at", keyTypes, valueType),
      Member("at index", List("Number"), valueType),
      Member("count", "Number"),
      Member("clear", "Nothing"),
      Member("copy", thisName)
    )
    else gAny(thisName) ::: List(
      Member("singleton", valueType)
    )

  def gIndexMember(thisName: String, fieldsAndKeys: List[Member]): List[Member] = gAny(thisName) ::: fieldsAndKeys ::: List(
    Member("clear fields", "Nothing")
  )

  def gCollection(thisName: String, typ: String): List[Member] = gAny(thisName) ::: List(
    Member("at", List("Number"), typ),
    Member("at index", List("Number"), typ),
    Member("copy", List(), thisName),
    Member("count", "Number"),
    Member("random", typ) // Gets a random item; invalid if collection is empty
  )

  def gRow(thisName: String, fields: List[Member]): List[Member] = gAny(thisName) ::: fields ::: List(
    Member("delete row", List(), "Nothing")
  )

  def gObject(thisName: String, fields: List[Member]): List[Member] = gAny(thisName) ::: fields ::: List(
    Member("clear fields", List(), "Nothing")
  )

  def gTable(thisName: String, typ: String): List[Member] = gCollection(thisName, typ) ::: List(
    Member("add row", List(), typ),
    Member("row at", List("Number"), typ),
    Member("clear", List(), "Nothing")
  )

  def gMutableCollection(thisName: String, typ: String): List[Member] = gCollection(thisName, typ) ::: List(
    Member("add", List(typ), "Nothing"),
    Member("add many", List(thisName), "Nothing"),
    Member("clear", "Nothing"),
    Member("contains", List(typ), "Boolean"), //Indicates if the collection contains the item
    Member("index of", List(typ, "Number"), "Number"),
    Member("insert at", List("Number", typ), "Nothing"),
    Member("random", typ),
    Member("remove", List(typ), "Boolean"),
    Member("remove at", List("Number"), "Nothing"),
    Member("reverse", "Nothing"),
    Member("@reverse", "Nothing"), // old name?
    Member("set at", List("Number", typ), "Nothing")
  )

}
