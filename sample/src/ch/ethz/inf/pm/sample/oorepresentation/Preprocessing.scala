package ch.ethz.inf.pm.sample.oorepresentation

import java.io.{File, StringWriter}

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
   Returns all possible candidates for a specific method name
    */
  def getMethods(name:String) : List[(ClassDefinition,MethodDeclaration)];

  /**
   This method returns the implementation of a given method

   @param name the name of the method
   @param classType the type of the class containing the method
   @param parameters the type of the parameters
   @return the implementation of the method and the class that contains it (it could be a superclass of the given class)
           or None if the method was not found
  */
	def getMethod(name : String, classType : Type, parameters : List[Type]) : Option[(MethodDeclaration, Type)];

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

  /**
   *  Reset
   */
  def reset()

  def compile(file:String) { compileFile(file) }
  def compile(file:File) { compileFile(file.getAbsolutePath) }
  def compile(files:List[String]) { files.foreach(compile _ ) }

}