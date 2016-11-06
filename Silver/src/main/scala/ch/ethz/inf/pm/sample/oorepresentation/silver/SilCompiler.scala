/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.oorepresentation.silver

import java.io.{BufferedReader, FileReader}
import java.nio.file.Files
import java.text.ParseException

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.oorepresentation.{Type, _}
import ch.ethz.inf.pm.sample.permissionanalysis.SilverSemantics
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.QuantifiedPermissionMethodSemantics
import viper.silver.ast.SourcePosition
import viper.silver.parser.{FastParser, _}
import viper.silver.verifier.ParseError
import viper.silver.{ast => sil}

import scala.io.Source

class SilCompiler extends Compiler {
  protected var classes: Option[List[ClassDefinition]] = None

  var program: sil.Program = null

  def label: String = "SIL"

  def extensions(): List[String] = "sil" :: Nil

  /**
    * @todo Does not support directories (multiple filbies) as input at the moment.
    * @todo Contains absolutely no error handling
    */
  def compile(comp: Compilable): List[ClassDefinition] = comp match {
    case Compilable.Path(file) =>
      val input = Source.fromInputStream(Files.newInputStream(file)).mkString
      val parseResult = FastParser.parse(input, file)
      val parsed = parseResult match {
        case fastparse.core.Parsed.Success(e: PProgram, _) => {
          e.initProperties()
          e
        }
        case fastparse.core.Parsed.Failure(msg, next, extra) =>
          throw new ParseException(s"$msg in $file at ${extra.line}:${extra.col}", 0)
        case ParseError(msg, pos) =>
          val (line, col) = pos match {
            case SourcePosition(_, line, col) => (line, col)
            case FilePosition(_, line, col) => (line, col)
            case _ => ??? // should never happen
          }
          throw new ParseException(s"$msg in $file at ${line}:${col}", 0)
      }

      Resolver(parsed).run

      val program = Translator(parsed).translate.get
      compileProgram(program)
    case _ => throw new UnsupportedOperationException("Compilable "+comp+" not supported by this compiler")
  }

  def compileProgram(p: sil.Program): List[ClassDefinition] = {
    SystemParameters.typ = TopType
    program = p
    classes = Some(DefaultSilverConverter.convert(p))
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
      RichNativeMethodSemantics :: QuantifiedPermissionMethodSemantics :: SilverSemantics :: Nil

  def reset(): Unit = {
    classes = None
  }

  // Copied from ScalaCompiler
  def getSourceCode(path: String): String =
    getOriginalCode(new BufferedReader(new FileReader(path)))

  def generateTopType(): Unit = {
    SystemParameters.typ = TopType
  }

  def refType: RefType =
    classes.get.head.typ.asInstanceOf[RefType]

}

