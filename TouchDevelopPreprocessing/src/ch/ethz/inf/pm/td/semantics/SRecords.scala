/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.Identifier
import ch.ethz.inf.pm.sample.oorepresentation.Modifier
import ch.ethz.inf.pm.td.analysis._
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of records
 *
 * Lists objects, tables and indexes defined in the current script
 *
 * @author Lucas Brutschy
 */
object SRecords extends ASingleton {

  lazy val typeName = TypeName("records",isSingleton = true)

  /** we use this to map strings stored in the string domain to identifiers for references */
  private val identRevMap = scala.collection.mutable.Map[String,Identifier]()
  def lookupRef(s:String) = identRevMap.get(s)
  def insertRef(s:Identifier) = identRevMap += (s.getName -> s)

  override def possibleFields = mutableFields.map(_._1)
  override def declarations = mutableDeclaration

  var mutableFields = Set.empty[(ApiField,Set[Modifier])]
  var mutableDeclaration = Map.empty[String,ApiMember]

  def addRecord(rec:Record) {
    val field = ApiField(rec.name,rec.typ)
    mutableFields = mutableFields + ((field,rec.modifiers))
    mutableDeclaration = mutableDeclaration ++ mkGetters(Set(field))
  }

  override def reset() = {
    mutableFields = Set.empty[(ApiField,Set[Modifier])]
    mutableDeclaration = Map.empty[String,ApiMember]
    identRevMap.clear
  }

  override def setUp(compiler:TouchCompiler,firstStart:Boolean): Unit = {

    mutableFields = mutableFields.map {
      x =>
        val initializer =
          if (firstStart) {
            if (x._2.contains(ResourceModifier) || x._2.contains(ReadOnlyModifier) || x._2(CloudEnabledModifier))
              TopInitializer
            else
              NewInitializer
          } else {
            if (x._2.contains(TransientModifier))
              NewInitializer
            else
              TopInitializer
          }
        (x._1.copy(default = initializer),x._2)
    }

    mutableDeclaration = mkGetters(mutableFields.map(_._1))

  }

}

case class Record(name:String, typ:AAny, modifiers:Set[Modifier])

      
