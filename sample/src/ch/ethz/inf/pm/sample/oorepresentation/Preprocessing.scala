package ch.ethz.inf.pm.sample.oorepresentation

import java.io.StringWriter

/**
 * A <code>Compiler</code> represents the interface to provide a compiler from a language to Simple
 *
 * @author Pietro Ferrara
 * @since 0.1
 */
trait Compiler {

  /**
   This method compiles the files contained in a path (that could be a file or a directory) and returns the list of
   all the class definitions contained in that path.

   @param path the path that contains the files to be compiled
   @return the list of the class definitions contained in the given path
  */
	def compileFile(path : String) : List[ClassDefinition];

  /**
   This method specifies which extensions are supported by this compiler

   @return the list of the extensions that are parsed by this compiler
  */
  def extensions() : List[String];

  /**
   This method returns a short description of the compiler.

   @return a short the description of the compiler (e.g., Java compiler)
  */
  def getLabel() : String;

  /**
   This method returns a list of definitions of the semantics of characteristic methods for the current language.

   @return the list of the semantics definitions
  */
  def getNativeMethodsSemantics() : List[NativeMethodSemantics];
}