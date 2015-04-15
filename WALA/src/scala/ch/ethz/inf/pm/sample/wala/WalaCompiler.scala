package ch.ethz.inf.pm.sample.wala

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import com.ibm.wala.classLoader.IMethod.SourcePosition
import com.ibm.wala.classLoader.{IField, IMethod, IClass}
import com.ibm.wala.ipa.cha.IClassHierarchy
import com.ibm.wala.types.TypeReference
import scala.collection._

import scala.collection.JavaConversions._

trait WalaType extends Type {
  override def isBottomExcluding(types: Predef.Set[Type]): Boolean = false
  override def bottom(): Type = WalaBottomType
  override def top(): Type = WalaTopType
  override def factory(): Type = WalaTopType
}

object WalaTopType extends WalaType with TopLattice[Type] {
  override def isObject: Boolean = false
  override def possibleFields: Predef.Set[Identifier] = Predef.Set.empty
  override def isBooleanType: Boolean = false
  override def isFloatingPointType: Boolean = false
  override def isStatic: Boolean = false
  override def isStringType: Boolean = false
  override def name: String = this.toString
  override def isNumericalType: Boolean = false
  override def arrayElementsType: Option[Type] = None
}

object WalaBottomType extends WalaType with BottomLattice[Type] {
  override def isObject: Boolean = false
  override def possibleFields: Predef.Set[Identifier] = Predef.Set.empty
  override def isBooleanType: Boolean = false
  override def isFloatingPointType: Boolean = false
  override def isStatic: Boolean = false
  override def isStringType: Boolean = false
  override def name: String = this.toString
  override def isNumericalType: Boolean = false
  override def arrayElementsType: Option[Type] = None
}

case class WalaInnerType(typ:TypeReference) extends WalaType with InnerLattice[Type,WalaInnerType] {

  override def lessEqualInner(other: WalaInnerType): Boolean = this.lubInner(other) == other

  override def wideningInner(other: WalaInnerType): Type = this.lubInner(other)

  override def glbInner(other: WalaInnerType): Type = {
    if (other.lessEqual(this)) other
    else if (this.lessEqual(other)) this
    else bottom()
  }

  override def lubInner(other: WalaInnerType): Type = {
    WalaInnerType(WalaCompiler.classHierarchy.getLeastCommonSuperclass(this.typ,other.typ))
  }

  override def isObject: Boolean = typ.isClassType
  override def possibleFields: Predef.Set[Identifier] = ???
  override def isBooleanType: Boolean = typ == TypeReference.Boolean
  override def isFloatingPointType: Boolean = typ == TypeReference.Float || typ == TypeReference.Double
  override def isStatic: Boolean = false
  override def isStringType: Boolean =  typ == TypeReference.JavaLangString
  override def name: String = typ.getName.toString
  override def isNumericalType: Boolean = isFloatingPointType || typ == TypeReference.Short || typ == TypeReference.Int || typ == TypeReference.Byte || typ == TypeReference.Long || typ == TypeReference.Char
  override def arrayElementsType: Option[Type] = None // FIXME

}

case class WalaPackageIdentifier(name:String) extends PackageIdentifier

case class WalaClassIdentifier(typ:WalaInnerType) extends ClassIdentifier {
  override def getThisType(): Type = typ
}

case class WalaProgramPoint(pos:SourcePosition) extends ProgramPoint {

  override def description: String = pos.toString

}

case class WalaMethodIdentifier(name:String) extends MethodIdentifier

/**
 * @author Lucas Brutschy
 */
object WalaCompiler extends Compiler {

  val classes:mutable.Set[ClassDefinition] = mutable.Set.empty
  var classHierarchy:IClassHierarchy = null

  def compile(method: IMethod, clazz:ClassDefinition): MethodDeclaration = {
    val programpoint: ProgramPoint = DummyProgramPoint
    val ownerType: Type = clazz.typ
    val modifiers: List[Modifier] =
        (if (method.isFinal)     List[Modifier](FinalModifier)     else List.empty[Modifier]) :::
        (if (method.isPrivate)   List[Modifier](PrivateModifier)   else List.empty[Modifier]) :::
        (if (method.isProtected) List[Modifier](ProtectedModifier) else List.empty[Modifier]) :::
        (if (method.isStatic)    List[Modifier](StaticModifier)    else List.empty[Modifier]) :::
        (if (method.isAbstract)  List[Modifier](AbstractModifier)  else List.empty[Modifier]) :::
        (if (method.isNative)    List[Modifier](NativeModifier)    else List.empty[Modifier]) :::
        (if (method.isInit)      List[Modifier](InitModifier)      else List.empty[Modifier])
    val name: MethodIdentifier = WalaMethodIdentifier(method.getName.toString)
    val parametricType: List[Type] = Nil
    val arguments: List[List[VariableDeclaration]] = List(
      (for ( i <- 0 to method.getNumberOfParameters-1) yield {
        val pp = WalaProgramPoint(method.getParameterSourcePosition(i))
        val typ: Type = WalaInnerType(method.getParameterType(i))
        val variable: Variable = Variable(pp,VariableIdentifier(i.toString)(typ,pp))
        val right: Option[Statement] = None
        VariableDeclaration(pp,variable,typ,right)
      }).toList
    )
    val returnType: Type = WalaInnerType(method.getReturnType)
    val body: ControlFlowGraph = null // TODO
    val precond: Statement = null
    val postcond: Statement = null
    val classDef: ClassDefinition = clazz

    new MethodDeclaration(programpoint, ownerType, modifiers, name, parametricType, arguments, returnType, body, precond,
      postcond, classDef)
  }

  def compile(field:IField): FieldDeclaration = {
    val programPoint: ProgramPoint = DummyProgramPoint
    val modifiers: List[Modifier] =
        (if (field.isFinal)     List[Modifier](FinalModifier)     else List.empty[Modifier]) :::
        (if (field.isPrivate)   List[Modifier](PrivateModifier)   else List.empty[Modifier]) :::
        (if (field.isProtected) List[Modifier](ProtectedModifier) else List.empty[Modifier]) :::
        (if (field.isStatic)    List[Modifier](StaticModifier)    else List.empty[Modifier])

    val typ: Type = WalaInnerType(field.getFieldTypeReference)
    val variable: Variable = Variable(programPoint,VariableIdentifier(field.getName.toString)(typ))
    val right: Option[Statement] = None

    new FieldDeclaration(programPoint,modifiers,variable,typ,right)
  }

  def compile(file:String, clazz: IClass): ClassDefinition = {
    val programPoint: ProgramPoint = DummyProgramPoint
    val typ: WalaInnerType = WalaInnerType(clazz.getReference)
    val modifiers: List[Modifier] = Nil
    val name: ClassIdentifier = WalaClassIdentifier(typ)
    val parametricTypes: List[Type] = Nil
    val extend: List[ClassIdentifier] =
      if (clazz.getSuperclass != null) WalaClassIdentifier(WalaInnerType(clazz.getSuperclass.getReference)) :: Nil
      else Nil
    val pack: PackageIdentifier = WalaPackageIdentifier(clazz.getName.getPackage.toString)
    val defi = new ClassDefinition(programPoint, typ, modifiers, name, parametricTypes, extend, Nil, Nil, pack, null)
    defi.fields  = (clazz.getAllFields map compile).toList
    defi.methods = (clazz.getAllMethods map {x => compile(x,defi)}).toList
    defi
  }

  override def compileFile(path: String): List[ClassDefinition] = {
    val engine = ScanDroidEngine.makeFromJar(path)
    classHierarchy = engine.cg.getClassHierarchy
    val res = for (clazz <- classHierarchy.iterator()) yield {
      compile(path, clazz)
    }
    classes ++= res
    res.toList
  }

  override def allMethods: List[MethodDeclaration] = {
    (for (c <- classes) yield {
      c.methods
    }).flatten.toList
  }

  /**
  This method returns a short description of the compiler.

   @return a short the description of the compiler (e.g., Java compiler)
    */
  override def getLabel(): String = "Wala Compiler"

  /**
  This method returns a list of definitions of the semantics of characteristic methods for the current language.

   @return the list of the semantics definitions
    */
  override def getNativeMethodsSemantics(): List[NativeMethodSemantics] = Nil

  /**
  This method specifies which extensions are supported by this compiler

   @return the list of the extensions that are parsed by this compiler
    */
  override def extensions(): List[String] = List("apk","jar")

  /**
   * This method returns the textual representation of the program BEFORE compiling it. This is useful to have some
   * statistics (e.g., LOC) of the original programs.
   */
  override def getSourceCode(path: String): String = ???

  /**
  Returns all possible candidates for a specific method name
    */
  override def getMethods(name: String): List[(ClassDefinition, MethodDeclaration)] = ???

  /**
   * Reset
   */
  override def reset(): Unit = {
    classes.clear()
    classHierarchy = null
  }

  /**
  This method returns the implementation of a given method

   @param name the name of the method
  @param classType the type of the class containing the method
  @param parameters the type of the parameters
  @return the implementation of the method and the class that contains it (it could be a superclass of the given class)
           or None if the method was not found
    */
  override def getMethod(name: String, classType: Type, parameters: List[Type]): Option[(MethodDeclaration, Type)] = ???
}
