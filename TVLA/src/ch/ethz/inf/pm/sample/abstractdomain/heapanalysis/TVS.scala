package ch.ethz.inf.pm.sample.abstractdomain.heapanalysis

import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.sample.abstractdomain.{HeapIdentifier, Identifier}


//================================================================================
// Three-Valued Structures
//================================================================================

/**
 * Kleene represents the truth values of Kleene's 3-valued logic
 */
object Kleene extends Enumeration {
  type Kleene = Value
  val True = Value("1")
  val False = Value("0")
  val Unknown = Value("1/2")
}

import Kleene._


/**
 * TVS models our version of three-valued structures as used by TVLA
 *
 * It is parameterized with the type of node names it contains
 * (usually these are the identifiers from our heap name scheme, but they can also be simple strings)
 */
class TVS[N <: NodeName] {

  /**
   * Heap nodes / individuals
   */
  var nodes: List[N] = List.empty

  /**
   * Program variable predicates
   */
  var programVariables: Map[String, ProgramVariablePredicate[N]] = Map.empty

  /**
   * Field predicates
   */
  var fields: Map[String, FieldPredicate[N]] = Map.empty

  /**
   * Naming predicates
   */
  var names: Map[String, NamePredicate[N]] = Map.empty

  /**
   * Summarization predicate
   */
  var summarization: UnaryPredicate[N] = new UnaryPredicate[N]("sm", Map.empty)

  /**
   * Transitive reachability instrumentation predicates
   */
  var transitiveReachability: Map[String, BinaryPredicate[N]] = Map.empty

  /**
   * Reachability from variables instrumentation predicates
   */
  var variableReachability: Map[String, UnaryPredicate[N]] = Map.empty

  /**
   * Shared-ness instrumentation predicate
   */
  var sharing: Map[String, UnaryPredicate[N]] = Map.empty



  /**
   * Renaming of the heap nodes to other names.
   * TVLA gives us a TVS with names that do not make much sense.
   * Later all the nodes can be translated to names according to our scheme.
   */
  def translateNames[M <: NodeName](tf: (N) => M): TVS[M] = {
    val result = new TVS[M]
    result.nodes = nodes map tf
    result.programVariables = programVariables mapValues {
      _.translateNames(tf)
    }
    result.fields = fields mapValues {
      _.translateNames(tf)
    }
    result.names = names mapValues {
      _.translateNames(tf)
    }
    result.summarization = summarization.translateNames(tf)

    result.transitiveReachability = transitiveReachability mapValues {
      _.translateNames(tf)
    }

    result.variableReachability = variableReachability mapValues {
      _.translateNames(tf)
    }


    result.sharing = sharing mapValues {
      _.translateNames(tf)
    }

    result
  }

  /**
   * Adds a program variable predicate
   */
  def addVariable(varName: String): TVS[N] = {
    val result = this.copy
    assert(!this.programVariables.contains(varName), "Predicate for variable " + varName + " exists already")
    result.programVariables += varName -> new ProgramVariablePredicate(varName, None)
    result
  }

  /**
   * Removes a program variable predicate
   */
  def removeVariable(varName: String): TVS[N] = {
    val result = this.copy
    assert(this.programVariables.contains(varName), "Predicate for variable " + varName + " doesnt exist")
    result.programVariables -= varName
    result.variableReachability = result.variableReachability filter { e => !e._1.contains(","+varName) }
    result
  }

  /**
   * Adds a new field predicate
   */
  def addField(field: String): TVS[N] = {
    val result = this.copy
    assert(!this.fields.contains(field))

    // add the new field predicate
    result.fields += field -> new FieldPredicate(field, Map())

    // transitive _reflexive_ reachability requires that all nodes reach themselves along the new field.
    // we need to update those instrumentation predicates, otherwise TVLA gets mad.
    val ts = for (n <- nodes) yield {
      // if the node is summarized, add a 1/2 self-loop, otherwise a normal one.
      val t = if (summarization.values.contains(n)) Unknown else True
      ((n, n), t)
    }
    val tname = "t["+field +"]"
    result.transitiveReachability += tname -> new BinaryPredicate(tname, ts.toMap)

    val sname = "is["+field +"]"
    result.sharing += sname -> new UnaryPredicate(sname, Map())


    for ((_,v) <- programVariables) {
      val rn = "r[" + field + "," + v.name + "]"
      val rv = for (n <- nodes; t <- v.value) yield
        (t, if (summarization.values.contains(t)) Unknown else True)
      result.variableReachability += rn -> new UnaryPredicate(rn, rv.toMap)
    }

    result
  }

  /**
   * Shallow copy of a TVS
   */
  def copy: TVS[N] = {
    val result = new TVS[N]
    result.nodes = this.nodes
    result.programVariables = this.programVariables
    result.fields = this.fields
    result.names = this.names
    result.summarization = this.summarization
    result.transitiveReachability = this.transitiveReachability
    result.variableReachability = this.variableReachability
    result.sharing = this.sharing
    result
  }

  override def equals(that: Any): Boolean = that match {
    case that: TVS[N] =>
      (this eq that) ||
        (this.nodes == that.nodes &&
          this.programVariables == that.programVariables &&
          this.fields == that.fields &&
          this.names == that.names &&
          this.summarization == that.summarization &&
          this.transitiveReachability == that.transitiveReachability &&
          this.variableReachability == that.variableReachability &&
          this.sharing == that.sharing)
    case _ => false
  }


  /**
   * Printing of TVS in the format used by TVLA. This is used as input to TVLA.
   */
  override def toString = {
    val space = " " * 2

    def indent(lines: List[String]) = lines map {
      space + _
    }
    def pm(m: Map[String, TVLAPredicate[N]]): List[String] = {
      val result =
        for (p <- (m.values.toList sortBy {
          _.name
        }))
        yield indent(p.toPrettyString)
      result.flatten
    }

    val buf = scala.collection.mutable.ListBuffer[String]()

    buf += "// nodes (universe)"
    buf += "%n = {" + nodes.mkString(",") + "}"

    buf += "%p = {"
    buf += space + "// program variable predicates"
    buf ++= pm(programVariables)
    buf += space + "// field predicates"
    buf ++= pm(fields)
    buf += space + "// summarization"
    buf ++= indent(summarization.toPrettyString)
    if (!names.isEmpty) {
      buf += space + "// naming predicates"
      buf ++= pm(names)
    }
    buf += space + "// transitive reachability"
    buf ++= pm(transitiveReachability)
    buf += space + "// variable reachability"
    buf ++= pm(variableReachability)
    buf += space + "// sharing"
    buf ++= pm(sharing)

    buf += "}"

    buf.mkString("\n")
  }
}



//================================================================================
// Predicate types used by our TVS.
//================================================================================

/**
 * A general predicate
 */
abstract class TVLAPredicate[N](val name: String) {
  def toPrettyString: List[String]

  def translateNames[M <: NodeName](tf: (N) => M): TVLAPredicate[M]
}

/**
 * General unary predicate (may point to several nodes)
 */
class UnaryPredicate[N](n: String, val values: Map[N, Kleene]) extends TVLAPredicate[N](n) {

  def +(kv: (N, Kleene)): UnaryPredicate[N] = {
    new UnaryPredicate(n, values + kv)
  }

  def pointsTo: Set[(N, Kleene)] = {
    values.toSet
  }

  def doesPointTo(node: N): Kleene = {
    values.get(node) match {
      case Some(truth) => truth
      case None => False
    }
  }

  def translateNames[M <: NodeName](tf: (N) => M): UnaryPredicate[M] =
    new UnaryPredicate[M](name, values map {
      v => (tf(v._1), v._2)
    })


  def toPrettyString: List[String] = {
    val v = values.map(e => e._1 + ":" + e._2)
    val result = name + " = { " + v.mkString(", ") + " }"

    List(result)
  }

  override def equals(that: Any): Boolean = that match {
    case that: UnaryPredicate[N] =>
      (this eq that) ||
        (this.name == that.name &&
          this.values == that.values)
    case _ => false
  }

  override def toString = {
    toPrettyString.mkString("\n")
  }
}

/**
 * Unary non-abstraction predicates for naming purposes
 */
class NamePredicate[N](n: String, values: Map[N, Kleene]) extends UnaryPredicate[N](n, values) {
  def this(name: String, node: N) = this (name, Map(node -> True))

  override def translateNames[M <: NodeName](tf: (N) => M): NamePredicate[M] =
    new NamePredicate[M](name, values map {
      v => (tf(v._1), v._2)
    })

}

/**
 * Program variable predicate which is unique (holds for / points to at most one node)
 */
class ProgramVariablePredicate[N](n: String, val value: Option[N]) extends TVLAPredicate[N](n) {
  def toPrettyString: List[String] = {
    val result = value map {
      _ + ":" + True
    }

    List(name + " = { " + result.getOrElse("") + " }")
  }

  def translateNames[M <: NodeName](tf: (N) => M): ProgramVariablePredicate[M] =
    new ProgramVariablePredicate(name, value map tf)

  override def equals(that: Any): Boolean = that match {
    case that: ProgramVariablePredicate[N] =>
      (this eq that) ||
        (this.name == that.name &&
          this.value == that.value)
    case _ => false
  }

  override def toString = {
    toPrettyString.mkString("\n")
  }
}


/**
 * Binary _function_ predicates for fields
 */
class FieldPredicate[N](n: String, val values: Map[N, (N, Kleene)]) extends TVLAPredicate[N](n) {
  def getNeighbour(n: N): Option[(N, Kleene)] = {
    values.get(n)
  }

  def setNeighbour(left: N, right: N, truth: Kleene): FieldPredicate[N] = {
    val entry = left -> (right, truth)
    new FieldPredicate[N](n, values + entry)
  }

  def toPrettyString: List[String] = {
    val v = values.map {
      case (l, (r, t)) => l + "->" + r + ":" + t
    }
    val result = name + " = { " + v.mkString(", ") + " }"

    List(result)
  }

  def translateNames[M <: NodeName](tf: (N) => M): FieldPredicate[M] =
    new FieldPredicate(name, values map {
      case (l, (r, t)) => (tf(l), (tf(r), t))
    })

  override def equals(that: Any): Boolean = that match {
    case that: FieldPredicate[N] =>
      (this eq that) ||
        (this.name == that.name &&
          this.values == that.values)
    case _ => false
  }

  override def toString = {
    toPrettyString.mkString("\n")
  }
}

/**
 * General binary predicates
 */
class BinaryPredicate[N](val n: String, val values: Map[(N, N), Kleene]) extends TVLAPredicate[N](n) {


  def toPrettyString: List[String] = {
    val v = values.map {
      case ((l, r), t) => l + "->" + r + ":" + t
    }
    val result = name + " = { " + v.mkString(", ") + " }"

    List(result)
  }

  def translateNames[M <: NodeName](tf: (N) => M): BinaryPredicate[M] =
    new BinaryPredicate(name, values map {
      case ((l, r), t) => ((tf(l), tf(r)), t)
    })

  override def equals(that: Any): Boolean = that match {
    case that: BinaryPredicate[N] =>
      (this eq that) ||
        (this.name == that.name &&
          this.values == that.values)
    case _ => false
  }

  override def toString = {
    toPrettyString.mkString("\n")
  }
}


//================================================================================
// Naming for nodes
//================================================================================

/**
 * NodeName can be used to name all individuals in a TVS
 */
abstract class NodeName extends HeapIdentifier[NodeName](null, null) {
  def representSingleVariable(): Boolean = false
  def getField(): Option[String] = null
  def getName(): String = null
}

/**
 * SimpleNode is just a string used as a name. E.g. the meaningless names produced by TVLA
 * like "_1" are all such names.
 */
case class SimpleNode(name: String) extends NodeName {
  override def toString = name
}

/**
 * PPHeapID is a node name conforming to our naming scheme (with PPs, counters etc.)
 */
case class PPHeapID(pps: Set[(ProgramPoint,Int)], unique: Int) extends NodeName {
  def this(pp: ProgramPoint, c: Int) = this(Set((pp,c)), 0)

  override def toString: String = {
    val entryToString = (e:(ProgramPoint,Int)) => "L"+e._1.getLine + "C"+ e._1.getColumn + "_" + e._2

    val s = pps.map(entryToString).mkString("+")

    if (unique == 0) s else "(" + s + ")_U" + unique

  }
}
