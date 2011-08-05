package ch.ethz.inf.pm.sample.abstractdomain.heapanalysis

/**
 * TVPAction represents a TVP action that can be instantiated with parameters.
 * It is used to easily construct the actions used in a CFG to execute in TVLA.
 * An instance of this class both represents a TVP declaration of an action and a particular
 * invocation of this action.
 *
 * NOTE: This could be cleaned up by either creating an EDSL build the AST of a program
 *       that can be printed to a program OR with an external template text file (and do substitution)
 *       For simplicity, we construct actions from strings for now since we don't need the whole
 *       TVP language anyway.
 */
abstract class TVPAction {
  /**
   * Name of the action
   */
  def name: String

  /**
   * The parameters this action takes
   */
  def formalParameters: List[String]

  /**
   * Instantiation of these parameters
   */
  def actualParameters: List[String]

  /**
   * Helper method for the signature of this action
   */
  def signature: String = name + "(" + formalParameters.mkString(",") + ")"

  /**
   * Header of the action: For focus formulas and preconditions
   */
  def header: String = ""

  /**
   * Predicate update formulas
   */
  def updateFormulas: List[String]


  /**
   * Particular invocation of the action
   */
  def invocation = name + "(" + actualParameters.mkString(",") + ")"


  /**
   * The whole declaration of the action as it appears in TVP
   */
  def declaration: String = "%action " + signature + " {\n" +
                            header + "\n" + "{\n" + updateFormulas.mkString("\n" + " "*4) + "}\n" + "}\n"

  override def equals(that: Any) = that match {
    case that: TVPAction => this.invocation == that.invocation
    case _ => false
  }

  override def hashCode = this.invocation.hashCode
}


/**
 * Action for "target = null"
 */
class SetVariableNull(target: String) extends TVPAction {
  def name: String = "setVariableNull"
  def formalParameters = List("target")
  def actualParameters = List(target)

  def updateFormulas = List("target(v) = 0")

  override def toString = target + " = null"
}


/**
 * Action for "target = source"
 */
class CopyVariable(target: String, source: String) extends TVPAction {
  def name: String = "copyVariable"
  def formalParameters = List("target", "source")
  def actualParameters = List(target, source)

  def updateFormulas = List("target(v) = source(v)")

  override def header = "%f { source(v) }"

  override def toString = target + " = " + source
}


/**
 * Action for "target.field = null."
 */
class SetFieldNull(target: String, field: String) extends TVPAction {
  def name: String = "setFieldNull"
  def formalParameters = List("target", "field")
  def actualParameters = List(target, field)

  def updateFormulas = List("field(v_1, v_2) = field(v_1, v_2) & !target(v_1)")

  override def header = "%f { target(v) }\n"

  override def toString = target + "." + field + " = null"
}

/**
 * Action for "target.field = source."
 */
class SetField(target: String, field: String, source: String) extends TVPAction {
  def name: String = "setField"
  def formalParameters = List("target", "field", "source")
  def actualParameters = List(target, field, source)

  override def header = "%f { target(v), source(v) }"

  def updateFormulas = List("field(v_1, v_2) = field(v_1, v_2) | target(v_1) & source(v_2)")

  override def toString = target + "." + field + " = " + source
}

/**
 * Action for "destination = target.field"
 */
class ExtractField(destination: String, target: String, field: String) extends TVPAction {
  def name: String = "extractField"
  def formalParameters = List("destination", "target", "field")
  def actualParameters = List(destination, target, field)

  override def header = "%f { E(v_1,v_2) target(v_1) & field(v_1,v_2) & t[field](v_2,v) } \n"
  def updateFormulas = List("destination(v) = E(v_1) target(v_1) & field(v_1, v)")

  override def toString = destination + " = " + target + "." + field
}

/**
 * Action for "target = new Object"
 */
class CreateObject(target: String) extends TVPAction {
  def name: String = "createObject"
  def formalParameters = List("target")
  def actualParameters = List(target)

  override def header = "%new"

  /**
   *  NOTE: we need to provide update formulas for ALL instrumentation predicates here.
   *        TVLA is not able to infer them in the case of object creation.
   */
  def updateFormulas = List("target(v) = isNew(v)",
  "foreach (f in Fields) {",
    "t[f](v_1, v_2) = (isNew(v_1) ? v_1 == v_2 : t[f](v_1, v_2))",
    "is[f](v) = is[f](v)",
    "r[f, target](v) = isNew(v)",
    "foreach(z in PVar-{target}) {",
      "r[f,z](v) = r[f,z](v)",
    "}",
  "}")

  override def toString = target + " = new Object()"
}

/**
 * Action for assumptions "if(v == null) ..."
 */
class AssumeVariableNull(v: String) extends TVPAction {
  def name: String = "assumeVariableNull"
  def formalParameters = List("var")
  def actualParameters = List(v)

  override def header = "%f { var(v) }\n" +
               "%p !(E(v) var(v))"
  def updateFormulas = Nil

  override def toString = v + " == null"
}

/**
 * Action for assumptions "if(v != null) ..."
 */
class AssumeVariableNotNull(v: String) extends TVPAction {
  def name: String = "assumeVariableNotNull"
  def formalParameters = List("var")
  def actualParameters = List(v)

  override def header = "%f { var(v) }\n" +
               "%p E(v) var(v)"
  def updateFormulas = Nil

  override def toString = v + " != null"
}

/**
 * Action for assumptions "if(var1 == var2) ..."
 */
class AssumeVariableEqual(var1: String, var2:String) extends TVPAction {
  def name: String = "assumeVariableEqual"
  def formalParameters = List("var1","var2")
  def actualParameters = List(var1,var2)

  override def header = "%f { var1(v), var2(v) } \n" +
               "%p A(v) var1(v) <-> var2(v)"
  def updateFormulas = Nil

  override def toString = var1 + " == " + var2
}


/**
 * Action for assumptions "if(var1 != var2) ..."
 */
class AssumeVariableNotEqual(var1: String, var2:String) extends TVPAction {
  def name: String = "assumeVariableNotEqual"
  def formalParameters = List("var1","var2")
  def actualParameters = List(var1,var2)

  override def header = "%f { var1(v), var2(v) } \n" +
               "%p !A(v) var1(v) <-> var2(v)"
  def updateFormulas = Nil

  override def toString = var1 + " != " + var2
}


/**
 * Action for least upper bound of two heap states
 */
class Lub extends TVPAction {
  def name: String = "lub"
  def formalParameters = List()
  def actualParameters = List()

  def updateFormulas = List()

  override def toString = "lub"
}





