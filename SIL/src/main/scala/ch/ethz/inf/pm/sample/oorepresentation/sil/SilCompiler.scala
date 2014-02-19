package ch.ethz.inf.pm.sample.oorepresentation.sil

import ch.ethz.inf.pm.sample.oorepresentation._
import java.io.{FileReader, BufferedReader}
import java.nio.file.{Files, Paths}
import scala.io.Source
import semper.sil.parser.Parser
import semper.sil.{ast => sil}
import ch.ethz.inf.pm.sample.oorepresentation.Type
import semper.sil.parser.Translator
import scala.Some
import semper.sil.parser.Resolver
import ch.ethz.inf.pm.sample.SystemParameters

class SilCompiler extends Compiler {
  protected var classes: Option[List[ClassDefinition]] = None

  var program: sil.Program = null

  def getLabel(): String = "SIL"

  def extensions(): List[String] = "sil" :: Nil

  /**
   * @todo Does not support directories (multiple files) as input at the moment.
   * @todo Contains absolutely no error handling
   */
  def compileFile(path: String): List[ClassDefinition] = {
    val file = Paths.get(path)
    val input = Source.fromInputStream(Files.newInputStream(file)).mkString
    val parseResult = Parser.parse(input, file)
    Resolver(parseResult.get).run

    val (program, _) = Translator(parseResult.get).translate
    compileProgram(program)
  }

  def compileProgram(p: sil.Program): List[ClassDefinition] = {
    program = p
    classes = Some(DefaultSilConverter.convert(p))
    classes.get
  }

  def allMethods: List[MethodDeclaration] =
    for (clazz <- classes.get; method <- clazz.methods) yield method

  def allMethodNames(): List[String] =
    for (clazz <- classes.get; method <- clazz.methods) yield method.name.toString

  def getMethods(name: String) =
    for (clazz <- classes.get; method <- clazz.methods; if method.name.toString == name) yield (clazz, method)

  def getMethod(name: String, classType: Type, parameters: List[Type]) = getMethods(name) match {
    case Nil => None
    // TODO: Should also check that the parameter types match etc.
    case (clazz, method) :: methods => Some(method, classType)
  }

  def getNativeMethodsSemantics() =
    ArithmeticAndBooleanNativeMethodSemantics ::
      RichNativeMethodSemantics :: Nil

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
    classes.get(0).typ.asInstanceOf[RefType]
}

