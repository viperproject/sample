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
            if (gd.modifiers.contains(ResourceModifier) || gd.modifiers.contains(ReadOnlyModifier)) // Art
              TopInitializer
            else // Persistent / Cloud-Enabled / Non-Persistent
              DefaultInitializer("Uninitialized")
          } else { // True for all executions
            if (gd.modifiers.contains(ResourceModifier) || gd.modifiers.contains(ReadOnlyModifier)) // Art
              TopInitializer
            else if (gd.modifiers.contains(gd.modifiers.contains(TransientModifier))) // Non-persistent
              DefaultInitializer("Uninitialized")
            else // Persistent / Cloud-Enabled
              TopWithInvalidInitializer("Uninitialized")
          }
        ApiField(gd.variable.getName,gd.typ.asInstanceOf[AAny],initializer,TopInitializer)
      }

    mutableDeclaration = mkGetterSetters(mutableFields)

  }

}
