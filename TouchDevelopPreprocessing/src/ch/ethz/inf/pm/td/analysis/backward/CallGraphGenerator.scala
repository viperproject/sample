package ch.ethz.inf.pm.td.analysis.backward

import ch.ethz.inf.pm.td.compiler.TouchCompiler
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.oorepresentation.MethodCall

/** A CallGraph edge (i.e. a single call from a call site to a method) */
case class CallEdge(source: MethodDeclaration, target: MethodDeclaration,
                pp: ProgramPoint, methodPos: MethodPosition, callStmt: MethodCall)

/** CallGraph represented as set of call edges */
case class CallGraph(calls: Seq[CallEdge], compiler: TouchCompiler) {

  /**
   * @return All the methods that can occur as "top level" calls (public methods like main for entering programs,
   *         events.) and that potentially lead to a call to mdecl
   */
  def callRoots(mdecl: MethodDeclaration): Seq[MethodDeclaration] = {
    val methods = compiler.allMethods
    methods
      .filter(compiler.isTopLevel)
      .filter(reachabilityMap(_).contains(mdecl)).toSeq
  }


  def calledFrom(m : MethodDeclaration): Set[MethodDeclaration] = {
    calls
      .filter(_.source == m)
      .map(_.target)
      .toSet
  }

  def reachableFrom(m: MethodDeclaration): Set[MethodDeclaration] = {
    var reached = Set.empty[MethodDeclaration]
    var toExplore = Set(m)
    while (!toExplore.isEmpty) {
      val cur = toExplore.head
      reached += cur
      toExplore -= cur
      val discovered = calledFrom(cur)
      toExplore ++= discovered diff reached
    }

    reached
  }

  /**
   * "a -> S"  in map means: all methods "m" in "S" potentially reachable from "a"
   */
  val reachabilityMap: Map[MethodDeclaration, Set[MethodDeclaration]] = {
    val methods = compiler.allMethods
    methods
      .map(m => m -> reachableFrom(m))
      .toMap
  }

}

/** Very crude call graph generator for TouchDevelop scripts */
class CallGraphGenerator(compiler: TouchCompiler) {

  /** Generates the call graph for a script */
  def generateForScript(script: ClassDefinition): CallGraph = {
    val calls = StatementCollector.collectClassStmts(methodCallFilter, script)
    CallGraph(calls, compiler)
  }

  private def resolveMethod(methodName: String): MethodDeclaration = {
    val matchingMethods = compiler.allMethodWithName(methodName)
      .filter(mdecl => mdecl.classDef == compiler.main)

    assert(matchingMethods.size == 1)
    matchingMethods.head
  }

  private def methodCallFilter(stmt: Statement)(loc: MethodPosition): Option[CallEdge] = stmt match {
    case mc: MethodCall =>
      mc.method match {
        case FieldAccess(pp, target, methodName, typ) if target.toString == "code" =>
          val method = resolveMethod(methodName)
          Some(CallEdge(source = loc.mdecl, target = method, pp, loc, mc))
        case _ => None
      }
    case _ => None
  }
}
