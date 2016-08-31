/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.Identifier
import ch.ethz.inf.pm.sample.oorepresentation.Modifier
import ch.ethz.inf.pm.td.analysis._
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * This is empty but needs to be there as a type
 *
 * @author Lucas Brutschy
 */

object SData extends ASingleton {

  lazy val typeName = TypeName("data",isSingleton = true)

  override def possibleFields = mutableFields.toSet[Identifier]
  override def declarations = mutableDeclaration

  var mutableFields = Set.empty[ApiField]
  var mutableDeclaration = Map.empty[String,ApiMember]

  override def reset() {
    mutableDeclaration = Map.empty[String,ApiMember]
    mutableFields = Set.empty[ApiField]
  }

  override def setUp(compiler:TouchCompiler,firstStart:Boolean) {

    mutableFields =
      for (gd <- compiler.globalData) yield {
        val initializer =
          if (firstStart) { // True for first execution
            // Art / Cloud-Enabled
            if (gd.modifiers.contains(ResourceModifier) || gd.modifiers.contains(ReadOnlyModifier)|| gd.modifiers.contains(CloudEnabledModifier))
              TopInitializer
            else // Persistent/ Non-Persistent
              DefaultInitializer("Uninitialized")
          } else { // True for all executions
            // Art / Cloud-Enabled TODO: Can cloud variables be invalid? Scripts seem to assume they are always valid
            if (gd.modifiers.contains(ResourceModifier) || gd.modifiers.contains(ReadOnlyModifier) || gd.modifiers.contains(CloudEnabledModifier))
              TopInitializer
            else if (gd.modifiers.contains(TransientModifier)) // Non-persistent
              DefaultInitializer("Uninitialized")
            else // Persistent
              TopWithInvalidInitializer("Uninitialized")
          }
        ApiField(gd.variable.getName,gd.typ.asInstanceOf[AAny],initializer,TopInitializer)
      }

    mutableDeclaration = mkGetterSetters(mutableFields)

  }

}
