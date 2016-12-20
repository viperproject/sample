/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.oorepresentation.sil

import java.io.{BufferedReader, FileReader}
import java.nio.file.Files

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.TypeMap
import ch.ethz.inf.pm.sample.oorepresentation.{Type, _}
import viper.silver.parser.{Parser, Resolver, Translator}
import viper.silver.{ast => sil}

import scala.io.Source

class SilCompiler extends Compiler {
  protected var classes: Option[List[ClassDefinition]] = None

  var program: sil.Program = _

  def label: String = "SIL"

  def extensions(): List[String] = "sil" :: Nil

  /**
   * @todo Does not support directories (multiple filbies) as input at the moment.
   * @todo Contains absolutely no error handling
   */
  def compile(comp: Compilable): List[ClassDefinition] = comp match {
    case Compilable.Path(file) =>
      val input = Source.fromInputStream(Files.newInputStream(file)).mkString
      val parseResult = Parser.parse(input, file)
      parseResult match {
        case Parser.Success(e, _) =>
          ()
        case Parser.Failure(msg, next) =>
          println(s"Failure: $msg $file, ${next.pos.line}, ${next.pos.column}")
          return Nil // FIXME
        case Parser.Error(msg, next) =>
          println(s"Error: $msg $file, ${next.pos.line}, ${next.pos.column}")
          return Nil // FIXME
      }
      Resolver(parseResult.get).run

      val program = Translator(parseResult.get).translate.get
      compileProgram(program)
    case _ => throw new UnsupportedOperationException("Compilable "+comp+" not supported by this compiler")
  }

  def compileProgram(p: sil.Program): List[ClassDefinition] = {
    SystemParameters.tm = SilTypeMap
    program = p
    classes = Some(DefaultSilConverter.convert(p))
    classes.get
  }

  def allMethods: List[MethodDeclaration] = classes.get.flatMap(_.methods)

  def allMethodNames(): List[String] =
    for (clazz <- classes.get; method <- clazz.methods) yield method.name.toString

  def getMethods(name: String) =
    for (clazz <- classes.get; method <- clazz.methods; if method.name.toString == name) yield (clazz, method)

  def getMethod(name: String, classType: Type, parameters: List[Type]) = getMethods(name) match {
    case Nil => None
    // TODO: Should also check that the parameter types match etc.
    case (clazz, method) :: methods => Some(method, classType)
  }

  def getNativeMethodsSemantics =
    ArithmeticAndBooleanNativeMethodSemantics ::
      RichNativeMethodSemantics :: Nil

  def reset(): Unit = {
    classes = None
  }

  // Copied from ScalaCompiler
  def getSourceCode(path: String): String =
    getOriginalCode(new BufferedReader(new FileReader(path)))

  def setUpTypes(): Unit = {
    SystemParameters.tm = SilTypeMap
  }

  def refType: RefType =
    classes.get.head.typ.asInstanceOf[RefType]

}


object SilTypeMap extends TypeMap {
  override val Int: Type = IntType
  override val Float: Type = TopType
  override val String: Type = TopType
  override val Boolean: Type = BoolType
  override val Bottom: Type = BottomType
  override val Top: Type = TopType
}
