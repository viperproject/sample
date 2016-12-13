/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package ch.ethz.inf.pm.td.cloud.boundedgraph

import ch.ethz.inf.pm.sample.oorepresentation.WeightedGraph
import ch.ethz.inf.pm.td.cloud.boundedgraph.Graph._
import ch.ethz.inf.pm.td.cloud.{DumpGraph, Z3Prover}

import scala.util.Random

object Encoder {

  def findViolations(g: Graph, sessionName:Option[String] = None): Option[WeightedGraph[String,String]] = {

    var violation:Option[WeightedGraph[String,String]] = None

    Z3Prover.withZ3[Unit,Expr,Var] ( { z3 =>

      phi(g,z3)
      z3.check() match {
        case Z3Prover.Sat =>
          val model = z3.extractModel()
          println(model.mkString("\n"))
          val cycle = toGraph(model)(g)
          val session = sessionName.getOrElse(Random.alphanumeric.take(5).mkString(""))
          println(DumpGraph("cycle_" + session, cycle, EventGraphRenderer(g, cycle)))
          violation = Some(cycle)
        case Z3Prover.Unknown =>
          assert(false)
        case Z3Prover.Unsat =>
          z3.printUnsatCore()
      }

    }, EventGraphZ3Converter )

    violation
  }

  def toGraph(model: Map[String, String])(implicit g: Graph): LabeledGraph[String, String] = {

    val pg = new LabeledGraph[String, String]()

    g.transactions.map(TransactionNames.make).foreach(pg.addNode)
    val nodeMap = pg.nodes.zipWithIndex.toMap
    g.events.foreach(x => pg.addNode(x.id))
    var nodeLabels = Map.empty[String, Map[String, String]]

    model.foreach {
      case (edgeString, "true") if EdgeNames.isName(edgeString) =>
        val (label,src,tgt) = EdgeNames.deconstruct(edgeString)
        if (label != "td")
          pg.addEdge(nodeMap(TransactionNames.make(src)), nodeMap(TransactionNames.make(tgt)), Some(label))
      case (edgeString, "false") if EdgeNames.isName(edgeString) =>
        ()
      case (nodeString, right) if EventArgumentNames.isName(nodeString) =>
        val (node, sort, n) = EventArgumentNames.deconstruct(nodeString)
        nodeLabels = nodeLabels + (node -> (nodeLabels.getOrElse(node, Map.empty) + (sort + n -> right)))
      case x =>
        println("skipping "+x)
    }

    nodeLabels.foreach(x => pg.setLabel(x._1, x._2.map(y => y._1 + "=" + y._2).mkString(",")))

    pg
  }

  def phi(implicit g:Graph, z3:Z3Prover[Expr,Var]): Unit = {

    ::("==== Axiomatic Model")

    ::("Po in Ca")
    for (e <- g.programOrder)
      assume(And(Ca(e.source.txn, e.target.txn), e.constraint.instantiate(e.source, e.target)))

    ::("Ca implies Ar")
    for (u <- g.transactions; v <- g.transactions if u <= v)
      assume( Implies(Ca(u,v), Ar(u,v)) )

    ::("Transitivity of Ca")
    for (u <- g.transactions; v <- g.transactions; w <- g.transactions if w != u && w != v)
      assume( Implies(And(Ca(u,w), Ca(w,v)), Ca(u,v)) )

    ::("Irreflexivity of Ca")
    for (u <- g.transactions)
      assume( Not(Ca(u,u)) )

    ::("Totality of Ar")
    for (u <- g.transactions; v <- g.transactions if u != v)
      assume(Or(Ar(u, v), Ar(v, u)))

    ::("Transitivity of Ar")
    for (u <- g.transactions; v <- g.transactions; w <- g.transactions if w != u && w != v)
      assume(Implies(And(Ar(u, w), Ar(w, v)), Ar(u, v)))

    ::("Irreflexivity of Ar")
    for (u <- g.transactions)
      assume(Not(Ar(u, u)))

    ::("==== Dependency Graph")

    for (e <- g.transactions; f <- g.transactions if e != f)
      assume(Iff(
        DSG(e, f),
        BigOr(
          // Program Order
          (if (g.programOrder.exists(x => x.source.txn == e && x.target.txn == f)) List(True) else Nil)
            :::
            // Update Conflicts
            (for (u <- g.updatesByTxn.getOrElse(e,Nil); v <- g.updatesByTxn.getOrElse(f,Nil)) yield
              And(Ar(u.txn, v.txn), Not(u commutesWith v)))
            :::
            // Dependencies
            (for (u <- g.updatesByTxn.getOrElse(e,Nil); q <- g.queriesByTxn.getOrElse(f,Nil)) yield
              BigAnd(
                Not(u commutesWith q) ::
                  Ca(u.txn, q.txn) ::
                  (for (v <- g.updates if v.txn != u.txn) yield {
                    Not(BigAnd(
                      Ar(u.txn, v.txn) :: Ca(v.txn, q.txn) :: List(u absorbedBy v)))
                  })
              )
              )
            :::
            // Anti-Dependencies
            (for (q <- g.queriesByTxn.getOrElse(e,Nil); u <- g.updatesByTxn.getOrElse(f,Nil)) yield
              BigAnd(
                Not(u commutesWith q) ::
                  Not(Ca(u.txn, q.txn)) ::
                  (for (v <- g.updates if v.txn != u.txn) yield {
                    Not(BigAnd(
                      Ar(u.txn, v.txn) :: Ca(v.txn, q.txn) :: List(u absorbedBy v)))
                  })
              )
              )
        )
      ))

    ::("==== Serializability")

    ::("Transitivity of TD")
    for (u <- g.transactions; v <- g.transactions)
      assume( Iff( TDSG(u,v) ,
        BigOr(
          (if (u != v) List(DSG(u,v)) else Nil)
          :::
            (for (w <- g.transactions if u != w && v != w) yield {
              And(DSG(u, w), TDSG(w, v))
            })
        ))
      )

    ::("Reflexivity of D")
    assume( BigOr( for (t <- g.transactions) yield TDSG(t,t)) )

  }

  def ::(a:String)(implicit z3:Z3Prover[Expr,Var]): Unit = {
    z3.writeLine("; "+a)
  }

  def assume(a:Expr)(implicit z3:Z3Prover[Expr,Var]): Unit = {
    z3.assume(a)
  }

  case class EventGraphRenderer(g: Graph, lg: LabeledGraph[String, String]) extends DumpGraph.GraphRenderer[String, String] {
    override def clazz(node: String): String =
      if (TransactionNames.isName(node)) "coral" else "blue"

    override def name(node: String): String =
      if (TransactionNames.isName(node)) node else node + " (" + lg.getLabel(node) + ")"

    override def label(value: String): String = value

    override def partitioning(value: String): Option[String] =
      g.eventMap.get(value).map(x => TransactionNames.make(x.txn))
  }

  case class Ar(l: TransactionID, r: TransactionID) extends BoolVar {
    override def name: String = EdgeNames.make("ar", l, r)
  }

  case class Ca(l: TransactionID, r: TransactionID) extends BoolVar {
    override def name: String = EdgeNames.make("ca", l, r)
  }

  case class DSG(l: TransactionID, r: TransactionID) extends BoolVar {
    override def name: String = EdgeNames.make("d", l, r)
  }

  case class TDSG(l: TransactionID, r: TransactionID) extends BoolVar {
    override def name: String = EdgeNames.make("td", l, r)
  }

  object EventArgumentNames {

    def make(e: EventID, sort: String, n: Int): String =
      "a-" + e + "-" + sort + "-" + n

    def isName(a: String): Boolean =
      a.startsWith("a-")

    def deconstruct(str: String): (EventID, String, Int) = {
      val Array(a, c, d, e) = str.split("-")
      assert(a == "a")
      (c, d, e.toInt)
    }

  }

  private object EdgeNames {

    def make(label: String, l: TransactionID, r: TransactionID): String =
      "e-" + label + "-" + l + "-" + r

    def isName(a: String): Boolean =
      a.startsWith("e-")

    def deconstruct(str: String): (String, TransactionID, TransactionID) = {
      val Array(a, b, c, d) = str.split("-")
      assert(a == "e")
      (b, c, d)
    }

  }

  private object TransactionNames {

    def make(r: TransactionID): String =
      "txn_" + r

    def isName(a: String): Boolean =
      a.startsWith("txn_")

    def deconstruct(str: String): TransactionID = {
      str.stripPrefix("txn_")
    }

  }

}

object EventGraphZ3Converter extends Z3Prover.ExpressionConverter[Expr,Var] {
  override def name(v: Var): String = v.name
  override def sort(v: Var): String = v.sort
  override def vars(expr: Expr): Set[Var] = expr.vars
  override def convert(expr: Expr): String = expr.smt
}

case class LabeledGraph[Node, Weight]() extends WeightedGraph[Node, Weight] {

  private var labels = Map.empty[Node, String]

  def setLabel(a: Node, b: String): Unit = {
    labels = labels + (a -> b)
  }

  def getLabel(a: Node): String = {
    labels.getOrElse(a, "")
  }

}