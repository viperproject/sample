package ch.ethz.inf.pm.td.tools

import ch.ethz.inf.pm.td.compiler.{ScriptRetriever, TouchException}
import ch.ethz.inf.pm.td.parser.TableDefinition
import ch.ethz.inf.pm.td.webapi.ScriptQuery
import net.liftweb.json.MappingException

import scala.collection.mutable

/**
 * Finds all scripts using a specific construct
 */
object FindConstruct {

  val containItself: mutable.Map[String,Boolean] = mutable.Map.empty[String,Boolean]
  val containThroughDependencies: mutable.Map[String,Set[String]] = mutable.Map.empty[String,Set[String]]

  def main(args:Array[String]) {

    var i = 0
    for (s <- new ScriptQuery) {
      if (i%100 == 0) println("checked "+i+"...")
      try {
        val deps = s.librarydependencyids.filter(isCloudEnabled)
        containThroughDependencies += (s.id -> deps.toSet)
        if (isCloudEnabled(s.id)) {
          println(s.id + " " + s.name + " contains cloud types")
        }
        if (deps.nonEmpty) {
          println(s.id + " depends on libraries containing cloud types: " + deps.mkString(","))
        }
      } catch {
        case m:MappingException => println("failed to import "+s.id+": "+m.msg)
        case m:TouchException => println("failed to import "+s.id+": "+m.msg)
        case m:Throwable => println("some exception in "+s.id+": "+m.toString)
      }
      i = i + 1
    }


  }

  def isCloudEnabled(id:String): Boolean = {
    containItself.get(id) match {
      case Some(x) => x
      case None =>
        val scr = ScriptRetriever.getLocally(id)
        val res = scr.get._1.declarations.exists {
          case TableDefinition(ident, typeName, keys, fields, isCloudEnabled, isCloudPartiallyEnabled, isPersistent, isExported) =>
            isCloudEnabled || isCloudPartiallyEnabled
          case _ => false
        }
        containItself += (id -> res)
        res
    }
  }

}
