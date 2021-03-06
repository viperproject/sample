/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.oorepresentation.silver

import java.io.File

import ch.ethz.inf.pm.sample.oorepresentation._
import java.nio.file.{Files, Paths}
import java.text.ParseException

import ch.ethz.inf.pm.sample.SystemParameters

import scala.io.Source
import viper.silver.parser._
import viper.silver.{ast => sil}
import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.abstractdomain.TypeMap
import ch.ethz.inf.pm.sample.permissionanalysis.SilverSemantics
import viper.silver.ast.SourcePosition
import viper.silver.verifier.ParseError

/**
  * TODO: Make the silver compiler extend the compiler interface once the rest of Sample also uses the new control flow graph
  * TODO: Does not support directories (multiple files) as input at the moment.
  * TODO: Contains absolutely no error handling.
  */
object SilverCompiler {

  def compile(filename: String): sil.Program = {
    val file = new File(filename)
    val path = file.toPath
    val compilable = Compilable.Path(path)
    compile(compilable)
  }

  /**
    * Compiles the given compilable.
    *
    * @param compilable
    * @return
    */
  def compile(compilable: Compilable): sil.Program = {
    val (result, label) = compilable match {
      case Compilable.Path(file) =>
        val input = Source.fromInputStream(Files.newInputStream(file)).mkString
        (FastParser.parse(input, file), file)
      case Compilable.Code(label, input) =>
        // input will be parsed but we'll need to pass in a valid Path object anyway. Let's use the working dir for this.
        (FastParser.parse(input, Paths.get(".").toAbsolutePath().normalize()), label)
      case _ => throw new UnsupportedOperationException("Compilable " + compilable + " not supported by this compiler")
    }
    val parsed = result match {
      case fastparse.core.Parsed.Success(e: PProgram, _) =>
        e.initProperties()
        e
      case fastparse.core.Parsed.Failure(msg, next, extra) =>

        throw new ParseException(s"$msg in $label", 0)
      case ParseError(msg, pos) =>
        val (line, col) = pos match {
          case SourcePosition(_, line, col) => (line, col)
          case FilePosition(_, line, col) => (line, col)
          case _ => ??? // should never happen
        }
        throw new ParseException(s"$msg in $label at $line:$col", 0)
    }

    Resolver(parsed).run
    Translator(parsed).translate.get
  }

  def toSample(program: sil.Program): SilverProgramDeclaration = {
    SystemParameters.tm = SilverTypeMap
    DefaultSilverConverter.convert(program)
  }

  def getNativeMethodSemantics: List[NativeMethodSemantics] =
    ArithmeticAndBooleanNativeMethodSemantics :: RichNativeMethodSemantics :: SilverSemantics :: Nil
}

object SilverTypeMap extends TypeMap {
  override val Int: Type = IntType
  override val Float: Type = TopType
  override val String: Type = TopType
  override val Boolean: Type = BoolType
  override val Perm: Type = PermType
  override val Bottom: Type = BottomType
  override val Top: Type = TopType
}
