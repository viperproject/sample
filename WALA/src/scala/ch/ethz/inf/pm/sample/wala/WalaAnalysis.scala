package ch.ethz.inf.pm.sample.wala

/**
 * Implements the main analysis backed up with Wala
 *
 * @author Lucas Brutschy
 */
object WalaAnalysis  {

  def main(args:Array[String]) = {

    for (arg <- args) {
      WalaCompiler.compileFile(arg)
    }

  }

}
