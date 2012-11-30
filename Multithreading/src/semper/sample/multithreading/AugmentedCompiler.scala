package semper.sample.multithreading

import ch.ethz.inf.pm.sample.oorepresentation.scalalang.ScalaCompiler
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.oorepresentation.VariableDeclaration
import ch.ethz.inf.pm.sample.abstractdomain.Expression

/**
 * Created with IntelliJ IDEA.
 * User: Pietro
 * Date: 29/11/12
 * Time: 14.53
 * To change this template use File | Settings | File Templates.
 */
class AugmentedCompiler extends ScalaCompiler {

  override def getLabel() : String = "Augmented Scala"

  override def compileFile(path : String) : List[ClassDefinition] = {
    val notAugmented = super.compileFile(path);
    var augmented : List[ClassDefinition] = Nil;
    for (c <- notAugmented) {
      var methods : List[MethodDeclaration] = Nil;
      for(m <- c.methods)
        methods=methods ::: new MethodDeclaration(
          m.programpoint,
          m.ownerType,
          m.modifiers,
          m.name,
          m.parametricType,
          m.arguments,
          m.returnType,
          CFGPreProcessing.augmentCFG(m.body),
          m.precond,
          m.postcond
        ) :: Nil
      augmented=augmented ::: new ClassDefinition(
        c.programpoint,
        c.typ,
        c.modifiers,
        c.name,
        c.parametricTypes,
        c.extend,
        c.fields,
        methods,
        c.pack,
        c.inv
      ) :: Nil
    }
    return augmented;
  }

}
