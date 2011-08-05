package ch.ethz.inf.pm.sample.abstractdomain.heapanalysis

import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
import ch.ethz.inf.pm.sample.abstractdomain.{Identifier, Replacement}

/**
 * A TVP specifies the program for TVLA to execute.
 * This class also deals with the assignment of proper heap IDs to heap nodes and tracks any changes to the IDs.
 */
class TVP(heap: TVSHeap) {

  //================================================================================
  // Action declarations
  //================================================================================

  /**
   * Sequence of  TVP actions that are to be executed
   */
  var actions: List[TVPAction] = List.empty

  /**
   * simply adds an action
   */
  def addAction(action: TVPAction)  {
    actions = actions :+ action
  }


  //=========================================================================================================
  // Predicate declarations (note that these are DECLARATIONS and not valuations of predicates like in TVS)
  //=========================================================================================================

  /**
   * the variable predicates declarations (both normal and temporary variables)
   */
  var programVariables: Set[String] = (heap.variables ++ heap.tempVariables) map {
    _.name
  }

  /**
   * field predicate declarations
   */
  var fields: Set[String] = heap.fields map encodeFieldname

  /**
   * name predicate declarations
   */
  var names: Set[String] = Set.empty

  /**
   * summarization predicate declaration
   */
  var unaries: Set[String] = Set("sm")

  /**
   * transitive reachability predicate declarations
   */
  var transitiveReachability: Set[String] = fields map {"t["+ _ + "]"}

  /**
   * reachability-from-variable predicate declarations
   */
  var variableReachability: Set[String] = for(f <- fields; v <- programVariables) yield "r["+f+","+v+"]"

  /**
   * shared-ness predicate declarations
   */
  var sharing: Set[String] = fields map {"is["+ _ + "]"}

  def encodeFieldname(fieldname: String) = List("field", fieldname) mkString "_"


  /**
   * encode TVP as string that can be used as TVLA TVP input
   */
  def program: String = {
    val space = " " * 2
    val tvs = scala.collection.mutable.ListBuffer[String]()

    // ** core predicates ** //

    // declaration of program variable predicates (variables and temporaries)
    tvs += "%s PVar {" + programVariables.mkString(",") + "}"
    tvs += "foreach (z in PVar) { "
    tvs += space + "%p z(v_1) unique pointer"
    tvs += "}"

    // declaration of naming predicates (to track node names)
    tvs += "%s Names { " + names.mkString(",") + "}"
    tvs += "foreach (z in Names) { "
    tvs += space + "%p z(v_1) nonabs"
    tvs += "}"

    // declaration of field predicates
    tvs += "%s Fields {" + fields.mkString(",") + "}"
    tvs += "foreach (f in Fields) {"
    tvs += space + "%p f(v_1,v_2) function"
    tvs += "}"


    // ** instrumentation predicates ** //

    // declaration of predicates for transitive reflexive reachability
    tvs += "foreach (f in Fields) {"
    tvs += space + "%i t[f](v_1, v_2) = f*(v_1, v_2) transitive reflexive"
    tvs += space + "%r !t[f](v_1, v_2) ==> !f(v_1, v_2)"
    tvs += space + "%r !t[f](v_1, v_2) ==> v_1 != v_2"
    tvs += space + "%r E(v_1) (t[f](v_1, v_2) & t[f](v_1, v_3) & !t[f](v_2, v_3)) ==> t[f](v_3, v_2)"
    tvs += "}"

    // declaration of predicates for transitive sharing of nodes
    tvs += "foreach (f in Fields) {"
    tvs += space + "%i is[f](v) = E(v_1, v_2) (v_1 != v_2 & f(v_1, v) & f(v_2, v))"
    tvs += "}"

    tvs += "foreach (f in Fields) {"
    tvs += space + "foreach (z in PVar) {"
    tvs += space*2 + "%i r[f,z](v) = E(v_1) (z(v_1) & t[f](v_1, v))"
    tvs += space + "}"
    tvs += "}"

    tvs += "%%"


    // ** Actions ** //
    val actionDecl = actions.map(_.declaration).toSet
    tvs ++= actionDecl

    tvs += "%%"


    // ** Control Flow Graph ** //
    // format:  list of "LABEL1 action(param1,...) LABEL2"
    val cfg = for (i <- 0 until actions.length) yield {
      (if (i == 0) "entry" else "L" + i) + " " +
        actions(i).invocation + " " +
        (if (i == actions.length - 1) "exit" else "L" + (i + 1))
    }
    tvs ++= cfg

    tvs.mkString("\n")
  }


  //================================================================================
  // Tracking and translation of heap node names
  //================================================================================

 /*
  * We map our complex (with PP etc.) names to simple names (n1,n2, etc.) to encode structures.
  * TVLA does not like special characters. This map records how we translate them before the execution of TVLA.
  */
  var NodeName2TVSName: Map[NodeName, SimpleNode] = Map.empty

  /*
   * the reverse of the above map
   */
  def TVSName2NodeName(n: SimpleNode): NodeName = {
    val reversed = NodeName2TVSName.map(_.swap)
    reversed(n)
  }

  /**
   * the current PP in Simple. New node names are assigned based on this
   */
  var newPP: Option[ProgramPoint] = None

  /**
   * How many nodes were already created at this PP
   */
  var newPPNum: Int = 0


  /**
   * Encode the current heap state for TVLA (with simple string names)
   */
  def encodeHeap: List[TVS[SimpleNode]] =  (for (s <- heap.structures) yield encodeStructure(s)).toList

  /**
   * Encodes a structure such that names can be tracked and all names conform to the syntax of TVLA.
   */
  def encodeStructure(structure: TVS[NodeName]): TVS[SimpleNode] = {
    val namedStruct = structure.copy

    // naming -- create non-abstraction unary predicates to track node names
    for (n <- structure.nodes) {
      if (!NodeName2TVSName.contains(n)) {
        NodeName2TVSName += n -> new SimpleNode("n" + (1 + NodeName2TVSName.size))
      }

      val TVSname = NodeName2TVSName(n)
      names += TVSname.toString
      namedStruct.names += TVSname.toString -> new NamePredicate(TVSname.toString, n)
    }

    // translate the names of all nodes in the structure
    namedStruct translateNames NodeName2TVSName
  }


  /**
   * Takes the output of TVLA and compares it with the names we recorded before.
   * Based on this comparison, assign the new names and create a Replacement for any changes.
   */
  def updateNames(tvs: TVS[SimpleNode]): (Map[SimpleNode, NodeName], Replacement) = {
    var names: Map[SimpleNode, Set[NodeName]] = Map.empty
    for (n <- tvs.nodes) {
      val oldNames = for ( (_,np) <- tvs.names; if (np.values.contains(n))) yield TVSName2NodeName(SimpleNode(np.name))
      names += n -> oldNames.toSet
    }
    var newNames: Map[SimpleNode, NodeName] = Map()
    val rep = new Replacement

    // if a node name is currently not unique, make it unique by increasing its UID part
    def makeUnique(nn: NodeName): NodeName = {
      val takenNewNames = newNames.values.toSet
      if (!takenNewNames.contains(nn)) {
        nn
      } else {
        val uniqueName =
          nn match {
            case PPHeapID(pps, u) =>
              val us = newNames.values.collect { case PPHeapID(ppns, un) if (pps == ppns) => un}
              PPHeapID(pps, us.max + 1)
          }
        uniqueName
      }
    }

    // update RHS of a replacement entry if it exists
    def updateRepl(lhs: Set[NodeName], rhs: Set[NodeName])  {
      val k = Set[Identifier]() ++ lhs
      rep.value(k) = rep.value.getOrElse(k, Set[Identifier]()) ++ rhs
    }

    // nodes that are not pointed by any naming predicate - they must be new!
    // assign a NEW name based on the program point and pp counter
    for (n <- tvs.nodes; if(names(n).isEmpty)) {
      val newNodeName = PPHeapID(Set((newPP.get, newPPNum)), 0)
      newNames += n -> newNodeName
    }

    // all the nodes to which nothing special happened
    for (n <- tvs.nodes; if(names(n).size == 1)) {
      val oldName = names(n).head
      val newNodeName = makeUnique(oldName)
      updateRepl(Set(oldName), Set(newNodeName))
      newNames += n -> newNodeName
    }

    // handle node merges - record this information in the replacement
    for (n <- tvs.nodes; if(names(n).size > 1)) {
      val merged = names(n) reduceLeft merge
      val newNodeName = makeUnique(merged)
      updateRepl(names(n), Set(newNodeName))
      newNames += n -> newNodeName
    }

    // node names must be unique (per TVS)
    assert(newNames.size == newNames.values.toSet.size)
    (newNames, rep)
  }

  /**
   * How to merge node names in our naming scheme
   */
  def merge(n1: NodeName, n2: NodeName): NodeName = {
    (n1, n2) match {
      case (PPHeapID(subnodes1, u1), PPHeapID(subnodes2, u2)) =>
        val a = subnodes1.toMap
        val b = subnodes2.toMap
        val merged = MapUtil.mergeMaps(a, b)(math.min)
        val um = if (subnodes1 == subnodes2) math.min(u1,u2) else 0
        PPHeapID(merged.toSet, um)
      case _ => throw new Exception("Couldnt merge nodes")
    }
  }


  //================================================================================
  // TVP Execution
  //================================================================================
  /**
   * Execute this TVP and return the new heap together with a replacement of the IDs
   */
  def execute(): (TVSHeap,Replacement) = {

    val inTVS: List[TVS[SimpleNode]] = encodeHeap
    // invoke TVLA
    val tvsout: List[TVS[SimpleNode]] = TVLARunner.run(inTVS.mkString("\n"), this)

    // build the updated heap state
    var newstructs: Set[TVS[NodeName]] = Set.empty
    var repl: Replacement = new Replacement
    for (tvs <- tvsout) {
      // updated the node names according to naming scheme
      val (newNames, structRepl) = updateNames(tvs)

      // replacements are tracked for each structure individually, so we just take the upper bound
      repl = repl.lub(repl, structRepl)

      // translate the simple (string) names to the normal ones
      val s = tvs translateNames newNames

      // remove naming predicates as we only needed them temporarily to track names
      s.names = Map.empty

      newstructs += s
    }

    val resultHeap = new TVSHeap
    resultHeap.variables = heap.variables
    resultHeap.tempVariables = heap.tempVariables
    resultHeap.ppCreates = heap.ppCreates
    resultHeap.structures = newstructs
    resultHeap.fields = heap.fields

    // log this call (if logging enabled)
    TVLALogger.log(new LogEntry(actions, heap, inTVS, resultHeap, tvsout))

    (resultHeap, repl)
  }
}


