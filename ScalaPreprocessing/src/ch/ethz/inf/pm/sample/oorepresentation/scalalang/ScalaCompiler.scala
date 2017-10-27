/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.oorepresentation.scalalang

import java.io._

import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain.TypeMap
import ch.ethz.inf.pm.sample.oorepresentation._

import scala.tools.nsc._

object ScalaClasses {
  var classes: List[ClassDefinition] = Nil
}

class ScalaCompiler extends Compiler {

  private var parsedclasses: List[ClassDefinition] = Nil

  def label: String = "Scala"

  def extensions(): List[String] = "scala" :: Nil

  def getNativeMethodsSemantics: List[NativeMethodSemantics] = List(BooleanNativeMethodSemantics, IntegerNativeMethodSemantics, ObjectNativeMethodSemantics)

  def getSourceCode(path: String): String = getOriginalCode(new BufferedReader(new FileReader(path)))

  def getMethod(name: String, classType: Type, parameters: List[Type]): Option[(MethodDeclaration, Type)] = {
    getClassDeclaration(classType) match {
      case Some(classe) =>
        for (m <- classe.methods)
          if (m.name.toString.equals(name) && m.arguments.head.size == parameters.size) {
            var ok: Boolean = true
            if (m.arguments.size != 1) throw new ScalaException("Not yet supported")
            for (i <- m.arguments.head.indices) {
              if (!parameters.apply(i).lessEqual(m.arguments.head.apply(i).typ))
                ok = false
            }
            if (ok) return new Some[(MethodDeclaration, Type)]((m, classType))
          }
        for (ext <- classe.extend)
          getMethod(name, ext.getThisType(), parameters) match {
            case Some(s) => return Some(s);
            case None =>
          }
        None;
      case None => None;
    }
  }

  def getMethods(name: String): List[(ClassDefinition, MethodDeclaration)] =
    for (clazz <- parsedclasses; method <- clazz.methods; if method.name.toString == name) yield (clazz, method)

  override def allMethods: List[MethodDeclaration] =
    for (clazz <- parsedclasses; method <- clazz.methods) yield method

  def reset() {
    parsedclasses = Nil
  }

  def setUpTypes() {
    val suffix: String = ".scala"
    val file: File = File.createTempFile("Dummy", suffix)
    val className: String = file.getName.substring(0, file.getName.length - suffix.length)
    val source: String = "class " + className + " {}"

    val out: FileWriter = new FileWriter(file)
    out.write(source)
    out.close()

    val classes = compile(Compilable.Path(file.toPath))

    if (classes.nonEmpty) {
      SystemParameters.tm = ScalaTypeMap(classes.head.typ)
    }
    else {
      throw new Exception("Could not generate type information")
    }

    val classFile: File = new File(className + ".class")
    if (classFile.exists) classFile.delete
  }

  def compile(comp: Compilable): List[ClassDefinition] = {
    comp match {

      case Compilable.Path(path) =>

        SystemParameters.addNativeMethodsSemantics(ObjectNativeMethodSemantics :: IntegerNativeMethodSemantics :: BooleanNativeMethodSemantics :: Nil)
        //SystemParameters.typ is initialized inside the parser

        val settings = new Settings
        settings.classpath.value = System.getProperty("java.class.path")


        // WORK
        val command = new CompilerCommand(List(path.toAbsolutePath.toString), settings) {

          /** The command name that will be printed in in the usage message.
            * This is automatically set to the value of 'plugin.commandname' in the
            * file build.properties.
            */
          override val cmdName = "scala2cfg"

        }

        if (!command.ok)
          return Nil

        /** The version number of this plugin is read from the properties file
          */
        if (settings.version.value) {
          println(command.cmdName + " version 1.0")
          return Nil
        }
        if (settings.help.value) {
          println(command.usageMsg)
          return Nil
        }

        val runner = new PluginRunner(settings)
        val run = new runner.Run
        run.compile(command.files)
        parsedclasses = parsedclasses ::: ScalaClasses.classes
        ScalaClasses.classes

      case _ =>
        throw new UnsupportedOperationException("Can only read scala code from files")

    }

  }

  private def getClassDeclaration(t: Type): Option[ClassDefinition] = {
    for (c <- parsedclasses)
      if (c.typ.equals(t))
        return Some(c)
    None
  }

}

case class ScalaTypeMap(topType: Type) extends TypeMap {

  // TODO: Implement this, if scala backend is ever used.

  override val Int: Type = ???
  override val Float: Type = ???
  override val String: Type = ???
  override val Boolean: Type = ???
  override val Perm: Type = ???
  override val Bottom: Type = ???
  override val Top: Type = ???
}