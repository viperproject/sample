package ch.ethz.inf.pm.sample.oorepresentation.sil

import ch.ethz.inf.pm.sample.oorepresentation._
import java.io.{FileReader, BufferedReader}
import java.nio.file.{Files, Paths}
import ch.ethz.inf.pm.sample.permissionanalysis.PermissionMethodSemantics

import scala.io.Source
import viper.silver.parser.Parser
import viper.silver.{ast => sil}
import ch.ethz.inf.pm.sample.oorepresentation.Type
import viper.silver.parser.Translator
import viper.silver.parser.Resolver
import ch.ethz.inf.pm.sample.SystemParameters

class SilCompiler extends Compiler {
  protected var classes: Option[List[ClassDefinition]] = None

  var program: sil.Program = null

  def getLabel(): String = "SIL"

  def extensions(): List[String] = "sil" :: Nil

  /**
   * @todo Does not support directories (multiple filbies) as input at the moment.
   * @todo Contains absolutely no error handling
   */
  def compileFile(path: String): List[ClassDefinition] = {
    val file = Paths.get(path)
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
  }

  def compileProgram(p: sil.Program): List[ClassDefinition] = {
    SystemParameters.typ = TopType
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

  def getNativeMethodsSemantics() =
    ArithmeticAndBooleanNativeMethodSemantics ::
      RichNativeMethodSemantics :: PermissionMethodSemantics :: Nil

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

