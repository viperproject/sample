package ch.ethz.inf.pm.td.typecheck

import ch.ethz.inf.pm.td.parser.{Scope, TypeName}
import scala.collection
import util.parsing.input.Position

/**
 *
 * Lucas Brutschy
 * Date: 8/20/12
 * Time: 1:52 PM
 *
 */
trait FieldStatistics extends SymbolTable {

  private var counters:List[Counter] = Nil

  def addCounter(counter:Counter) { counters = counter :: counters }

  abstract override def resolveLib(lib:String, action:String, types:List[TypeName], pos:Position):List[TypeName] = {
    val ret = super.resolveLib(lib,action,types,pos)
    for (counter <- counters) {
      counter.countTypes(ret)
      counter.countFeature("Libaries")
    }
    ret
  }

  abstract override def resolveCode(action:String, types:List[TypeName], pos:Position):List[TypeName] = {
    val ret = super.resolveCode(action,types,pos)
    for (counter <- counters) counter.countTypes(ret)
    ret
  }

  abstract override def resolveLocal(scope:Scope, symbol:String, pos:Position):TypeName = {
    val ret = super.resolveLocal(scope,symbol,pos)
    for (counter <- counters) counter.countTypes(List(ret))
    ret
  }

  abstract override def resolveData(symbol:String, pos:Position):TypeName = {
    val ret = super.resolveData(symbol,pos)
    for (counter <- counters) counter.countTypes(List(ret))
    ret
  }

  abstract override def resolveAccess(typ:TypeName, symbol:String, args:List[TypeName]=Nil):TypeName = {
    super.resolveUsertypeAccess(typ,symbol,args) match {
      case Some(x) =>
        for (counter <- counters) counter.countFeature("User-Defined-Types")
        x
      case None =>
        for (counter <- counters) counter.countMember(typ+"."+symbol)
        val ret = super.resolveAccess(typ,symbol,args)
        for (counter <- counters) counter.countTypes(List(ret))
        ret
    }
  }

}

abstract class Counter {
  protected var curScript:String = null
  def setScript(script:String) {curScript = script;}
  def countMember (k:String)
  def countTypes (typeList:List[TypeName])
  def countFeature (feature:String)
}


class UsesCounter extends Counter {

  private val numberOfElements = 300
  private val numberOfTypes = 20
  private val numberOfFeatures = 2

  private val urls = collection.mutable.Set[String]()

  private val membersTotal = collection.mutable.HashMap[String,Int]()
  private val typesTotal = collection.mutable.HashMap[String,Int]()
  private val featuresTotal = collection.mutable.HashMap[String,Int]()

  private val membersScripts = collection.mutable.HashMap[String,Set[String]]()
  private val typesScripts = collection.mutable.HashMap[String,Set[String]]()
  private val featuresScripts = collection.mutable.HashMap[String,Set[String]]()

  override def setScript(url:String) {
    super.setScript(url)
    urls.add(url)
  }

  def countMember (k:String) {
    membersScripts(k) = (if (membersScripts.contains(k)) membersScripts(k) else Set.empty[String]) + curScript
    membersTotal(k) = (if (membersTotal.contains(k)) membersTotal(k) else 0) + 1
  }

  def countFeature (k:String) {
    featuresScripts(k) = (if (featuresScripts.contains(k)) featuresScripts(k) else Set.empty[String]) + curScript
    featuresTotal(k) = (if (featuresTotal.contains(k)) featuresTotal(k) else 0) + 1
  }

  def countTypes (typeList:List[TypeName]) {
    for (typ <- typeList) {
      val k = typ.toString
      typesScripts(k) = (if (typesScripts.contains(k)) typesScripts(k) else Set.empty[String]) + curScript
      typesTotal(k) = (if (typesTotal.contains(k)) typesTotal(k) else 0) + 1
    }
  }

  private def toStringSorted(map:collection.mutable.HashMap[String,Int], num:Int):String = {
    val tops = map.toList.sortWith (_._2 > _._2) slice (0,num)
    //val sortedTops = tops.sortWith ( (a1:(String,Int),a2:(String,Int)) => (a1._1 compareToIgnoreCase a2._1) < 0)
    var list = ""
    for ((el,cnt) <- tops) {
      list += "  %-30s %8d\n" format (el,cnt)
    }
    list
  }


  private def toStringPercentageSorted(map:collection.mutable.HashMap[String,Set[String]], num:Int):String = {
    val tops = map.toList.sortWith (_._2.size > _._2.size) slice (0,num)
    //val sortedTops = tops.sortWith ( (a1:(String,Int),a2:(String,Int)) => (a1._1 compareToIgnoreCase a2._1) < 0)
    var list = ""
    for ((el,set) <- tops) {
      list += "  %-30s %4.4f\n" format (el,set.size.toFloat/urls.size*100)
    }
    list
  }

  private def toCodeSorted(map:collection.mutable.HashMap[String,Set[String]], num:Int):String = {
    val tops = map.toList.sortWith (_._2.size > _._2.size) slice (0,num)
    val sortedTops = tops.sortWith ( (a1:(String,Set[String]),a2:(String,Set[String])) => (a1._1 compareToIgnoreCase a2._1) < 0)
    var list = ""
    for ((el,cnt) <- sortedTops) {
      list += "  \""+el+"\",\n"
    }
    list
  }

  override def toString = {
    "We have analyzed "+urls.size+" Scripts!!!\n" +
    "TOP " + numberOfElements + " members by total uses\n" + toStringSorted(membersTotal, numberOfElements) + "\n" +
    "TOP " + numberOfTypes + " types by total uses\n" + toStringSorted(typesTotal, numberOfTypes) + "\n" +
    "TOP " + numberOfFeatures + " features by total uses\n" + toStringSorted(featuresTotal, numberOfFeatures) + "\n" +
    "TOP " + numberOfElements + " members by percentage\n" + toStringPercentageSorted(membersScripts, numberOfElements) + "\n" +
    "TOP " + numberOfTypes + " types by percentage\n" + toStringPercentageSorted(typesScripts, numberOfTypes) + "\n" +
    "TOP " + numberOfFeatures + " features by percentage\n" + toStringPercentageSorted(featuresScripts, numberOfFeatures) + "\n" +
    "IN CODE THAT IS\n" +
    "val coveredMembers = List(\n" + toCodeSorted(membersScripts, numberOfElements) + ")\n" +
    "val coveredTypes = List(\n" + toCodeSorted(typesScripts, numberOfTypes) + ")\n" +
    "val coveredFeatures = List(\n" + toCodeSorted(featuresScripts, numberOfFeatures) + ")\n"
  }

}


class CoverageCounter extends UsesCounter {

  private val members = collection.mutable.HashSet[String]()
  private val types = collection.mutable.HashSet[String]()
  private val features = collection.mutable.HashSet[String]()
  private val coveredScripts = collection.mutable.HashSet[String]()
  private val uncoveredScripts = collection.mutable.HashSet[String]()

  def addMember(k:String) {members += k}
  def addType(t:String) {types += t}
  def addFeature(f:String) {features += f}

  def uncover() {
    if(coveredScripts.contains(curScript)) {
      coveredScripts.remove(curScript)
      uncoveredScripts.add(curScript)
    }
  }

  override def setScript(url:String) {
    super.setScript(url)
    coveredScripts.add(url)
  }

  override def countMember (k:String) {
    if (!members.contains(k)) { uncover(); super.countMember(k) }
  }

  override def countFeature (k:String) {
    if (!features.contains(k)) { uncover(); super.countFeature(k) }
  }

  override def countTypes (typeList:List[TypeName]) {
    for (typ <- typeList) {
      val k = typ.toString
      if (!types.contains(k)) { /*uncover();*/ super.countTypes(List(typ)) }
    }
  }

  override def toString = {
    val p = coveredScripts.size
    val q = (p + uncoveredScripts.size)
    "COVERED:covered "+p+" of "+q+" scripts, that's "+(p.toFloat/q*100)+"% of the scripts\n" +
    "TOP UNCOVERED: "+super.toString
  }

}