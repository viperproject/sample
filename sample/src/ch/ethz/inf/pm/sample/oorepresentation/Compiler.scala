/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.oorepresentation

import java.io.BufferedReader

/**
  * An entity that can be compiled into Simple
  */
sealed trait Compilable {

  /**
    * Some short name identifying the compilation unit (for output)
    */
  def label:String
}

object Compilable {

  /**
    * Compilation unit which directly provides code
    */
  case class Code(label:String, code:String) extends Compilable

  /**
    * Read compilation unit from directory or file
    */
  case class Path(path:java.nio.file.Path) extends Compilable {
    def label = path.getFileName.toString
  }

  /**
    * Address compilation unit by unique identifier
    */
  case class Identifier(id:String) extends Compilable {
    def label = id
  }

}

/**
  * A <code>Compiler</code> represents the interface to provide a compiler from a language to Simple
  *
  * @author Pietro Ferrara, Lucas Brutschy
  * @since 0.1
  */
trait Compiler {

  /**
    * This method compiles a <code>Compilable</code> and returns the list of
    * all the class definitions.
    *
    * @param comp the compilable that is to be compiled
    * @return the list of the class definitions contained in the given path
    */
  def compile(comp: Compilable): List[ClassDefinition]

  /**
    * Gives convenient access to all methods that were compiled with this compiler
    *
    * @return a list of all methods
    */
  def allMethods: List[MethodDeclaration]

  /**
    * Returns all possible candidates for a specific method name
    */
  def getMethods(name: String): List[(ClassDefinition, MethodDeclaration)]

  /**
    * This method returns the implementation of a given method
    * *
    *
    * @param name       the name of the method
    * @param classType  the type of the class containing the method
    * @param parameters the type of the parameters
    * @return the implementation of the method and the class that contains it (it could be a superclass of the given class)
    *         or None if the method was not found
    */
  def getMethod(name: String, classType: Type, parameters: List[Type]): Option[(MethodDeclaration, Type)]

  /**
    * This method specifies which extensions are supported by this compiler
    *
    * @return the list of the extensions that are parsed by this compiler
    */
  def extensions(): List[String]

  /**
    * This method returns a short description of the compiler.
    *
    * @return a short the description of the compiler (e.g., Java compiler)
    */
  def label: String

  /**
    * This method returns a list of definitions of the semantics of characteristic methods for the current language.
    *
    * @return the list of the semantics definitions
    */
  def getNativeMethodsSemantics: List[NativeMethodSemantics]

  /**
    * Reset
    */
  def reset()

  /**
    * This method returns the textual representation of the program BEFORE compiling it. This is useful to have some
    * statistics (e.g., LOC) of the original programs.
    */
  def getSourceCode(path: String): String

  def setUpTypes()

  protected def getOriginalCode(reader: BufferedReader): String = {
    var output = ""
    var newLine = reader.readLine()
    while (newLine != null) {
      output = output + newLine + "\n"
      newLine = reader.readLine()
    }
    output
  }

}