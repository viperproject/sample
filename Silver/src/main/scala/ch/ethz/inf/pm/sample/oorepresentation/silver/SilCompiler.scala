/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.oorepresentation.silver

import ch.ethz.inf.pm.sample.oorepresentation._
import java.io.{BufferedReader, FileReader}
import java.nio.file.{Files, Paths}
import java.text.ParseException

import scala.io.Source
import viper.silver.parser.{FastParser, _}
import viper.silver.{ast => sil}
import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.permissionanalysis.SilverSemantics
import viper.silver.ast.SourcePosition
import viper.silver.verifier.ParseError

/**
  * TODO: Make the silver compiler extend the compiler interface once the rest of Sample also uses the new control flow graph.
  */
class SilverCompiler {
  protected var prog: Option[SilverProgramDeclaration] = None

  /**
    * TODO: Does not support directories (multiple files) as input at the moment.
    * TODO: Contains absolutely no error handling.
    */
  def compile(compilable: Compilable): SilverProgramDeclaration = compilable match {
    case Compilable.Path(file) =>
      val input = Source.fromInputStream(Files.newInputStream(file)).mkString
      val parseResult = FastParser.parse(input, file)
      val parsed = parseResult match {
        case fastparse.core.Parsed.Success(e: PProgram, _) =>
          e.initProperties()
          e
        case fastparse.core.Parsed.Failure(msg, next, extra) =>
          throw new ParseException(s"$msg in $file at ${extra.line}:${extra.col}", 0)
        case ParseError(msg, pos) =>
          val (line, col) = pos match {
            case SourcePosition(_, line, col) => (line, col)
            case FilePosition(_, line, col) => (line, col)
            case _ => ??? // should never happen
          }
          throw new ParseException(s"$msg in $file at $line:$col", 0)
      }

      Resolver(parsed).run

      val program = Translator(parsed).translate.get
      compileProgram(program)
    case _ => throw new UnsupportedOperationException("Compilable " + compilable + " not supported by this compiler")
  }

  def compileProgram(program: sil.Program): SilverProgramDeclaration = {
    SystemParameters.typ = TopType
    val converted = DefaultSilverConverter.convert(program)
    this.prog = Some(converted)
    converted
  }

  def program: SilverProgramDeclaration =
    prog.get

  def allFunctions: Seq[SilverFunctionDeclaration] =
    program.functions

  def allMethods: Seq[SilverMethodDeclaration] =
    program.methods

  def getNativeMethodSemantics =
    ArithmeticAndBooleanNativeMethodSemantics :: RichNativeMethodSemantics :: SilverSemantics :: Nil
}
