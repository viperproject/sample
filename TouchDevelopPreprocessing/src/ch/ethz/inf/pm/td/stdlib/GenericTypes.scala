package ch.ethz.inf.pm.td.stdlib

import ch.ethz.inf.pm.td.symbols.Member


/**
 *
 * Lucas Brutschy
 * Date: 8/21/12
 * Time: 10:34 AM
 *
 */
object GenericTypes {

  def gAlsoSingletons(thisName:String):List[Member] = List(
    Member("∥",List("String"),"String"),
    Member("equals",List(thisName),"Boolean")
  )


  def gAny(thisName:String):List[Member] = gAlsoSingletons(thisName) ::: List(
    Member("is_invalid","Boolean"),
    Member("post_to_wall","Nothing")
  )

  def gIndex(thisName:String, keyTypes:List[String],valueType:String):List[Member] =
    if (keyTypes.size > 0) gAny(thisName) ::: List(
      Member("at",keyTypes,valueType),
      Member("at_index",List("Number"),valueType),
      Member("count","Number"),
      Member("clear","Nothing"),
      Member("copy",thisName)
    ) else gAny(thisName) ::: List(
      Member("singleton",valueType)
    )

  def gIndexMember(thisName:String,fieldsAndKeys:List[Member]):List[Member] = gAny(thisName) ::: fieldsAndKeys ::: List(
    Member("clear_fields","Nothing")
  )

  def gCollection(thisName:String,typ:String):List[Member] = gAny(thisName) ::: List(
    Member("at",List("Number"),typ),
    Member("at_index",List("Number"),typ),
    Member("copy",List(),thisName),
    Member("count","Number"),
    Member("random",typ)	// Gets a random item; invalid if collection is empty
  )

  def gField(thisName:String,typ:String):List[Member] = gAny(thisName) ::: List(
    Member("get",typ),
    Member("set",List(typ),"Nothing"),
    Member("clear",List(),"Nothing")
  )

  def gNumberField(thisName:String):List[Member] = gField(thisName,"Number") ::: List(
    Member("add",List("Number"),"Nothing")
  )

  def gRow(thisName:String,fields:List[Member]):List[Member] = gAny(thisName) ::: fields ::: List(
    Member("delete_row",List(),"Nothing")
  )

  def gObject(thisName:String,fields:List[Member]):List[Member] = gAny(thisName) ::: fields ::: List(
    Member("clear_fields",List(),"Nothing")
  )

  def gTable(thisName:String,typ:String):List[Member] = gCollection(thisName,typ) ::: List(
    Member("add_row",List(),typ),
    Member("clear",List(),"Nothing")
  )

  def gMutableCollection(thisName:String,typ:String):List[Member] = gCollection(thisName,typ) ::: List(
    Member("add",List(typ),"Nothing"),
    Member("add_many",List(thisName),"Nothing"),
    Member("clear","Nothing"),
    Member("contains",List(typ),"Boolean"),	//Indicates if the collection contains the item
    Member("index_of",List(typ,"Number"),"Number"),
    Member("insert_at",List("Number",typ),"Nothing"),
    Member("random",typ),
    Member("remove",List(typ),"Boolean"),
    Member("remove_at",List("Number"),"Nothing"),
    Member("reverse","Nothing"),
    Member("set_at",List("Number",typ),"Nothing")
  )

}
