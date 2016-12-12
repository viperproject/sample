/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.cloud

object BoundedEventGraph {

  type OperationID = String
  type TransactionID = String
  type EventID = String

  case class Graph (
      events: List[Event],
      programOrder: List[Edge],
      system: SystemSpecification
  ) {

    lazy val eventMap:Map[String,Event] = events.map{ x => (x.id,x )}.toMap
    lazy val operationMap:Map[String,Operation] = system.operations.map{ x => (x.id,x )}.toMap
    lazy val updates: List[Event] = events.filter(x => operationMap(x.operationID).isInstanceOf[Update])
    lazy val queries: List[Event] = events.filter(x => operationMap(x.operationID).isInstanceOf[Query])
    lazy val transactions: List[TransactionID] = events.map(_.txn).distinct
    lazy val updatesByTxn: Map[TransactionID,List[Event]] = updates.groupBy(_.txn)
    lazy val queriesByTxn: Map[TransactionID,List[Event]] = queries.groupBy(_.txn)

  }

  trait Expr {

    def instantiate(l:Event, r:Event):Expr =
      replace({
        case a: LeftArgVar  => a.bind(l)
        case a: RightArgVar => a.bind(r)
        case x => x
      })

    def replace(f:Var => Var):Expr
    def vars:Set[Var]

    def smt:String
    def smt2op(a:String, ops:String*): String = "("+a+" "+ops.mkString(" ")+")"

  }

  trait UnOp extends Expr {
    val e:Expr
    def factory(e:Expr):Expr
    override def replace(f: (Var) => Var):Expr = factory(e.replace(f))
    override def vars:Set[Var] = e.vars
  }

  trait BinOpExpr extends Expr {
    val l:Expr
    val r:Expr
    def factory(l:Expr, r:Expr):Expr
    override def replace(f: (Var) => Var):Expr = factory(l.replace(f),r.replace(f))
    override def vars:Set[Var] = l.vars ++ r.vars
  }

  trait ArbOp extends Expr {
    val es:List[Expr]
    def factory(es:List[Expr]):Expr
    override def replace(f: (Var) => Var):Expr = factory(es map { _.replace(f) })
    override def vars:Set[Var] = if (es.isEmpty) Set.empty else es.map(_.vars).reduce(_ ++ _)
  }

  trait Constant extends Expr {
    override def replace(f: (Var) => Var): Expr = this
    override def vars:Set[Var] = Set.empty
  }

  case class Implies(l:Expr,r:Expr) extends BinOpExpr {
    override def factory(l: Expr, r: Expr) = Implies(l,r)
    override def smt: String = smt2op("=>", l.smt, r.smt)
  }
  case class Iff(l:Expr,r:Expr) extends BinOpExpr {
    override def factory(l: Expr, r: Expr) = Iff(l,r)
    override def smt: String = smt2op("=", l.smt, r.smt)
  }
  case class And(l:Expr,r:Expr) extends BinOpExpr {
    override def factory(l: Expr, r: Expr) = And(l,r)
    override def smt: String = smt2op("and", l.smt, r.smt)
  }
  case class Or(l:Expr,r:Expr) extends BinOpExpr {
    override def factory(l: Expr, r: Expr) = Or(l,r)
    override def smt: String = smt2op("or", l.smt, r.smt)
  }
  case class Equal(l:Expr,r:Expr) extends BinOpExpr {
    override def factory(l: Expr, r: Expr) = Equal(l,r)
    override def smt: String = smt2op("=", l.smt, r.smt)
  }
  case class Unequal(l:Expr,r:Expr) extends BinOpExpr {
    override def factory(l: Expr, r: Expr) = Unequal(l,r)
    override def smt: String = smt2op("not",smt2op("=", l.smt, r.smt))
  }
  case class Not(e:Expr) extends UnOp {
    override def factory(e: Expr) = Not(e)
    override def smt: String =
      e match {
        case True => False.smt
        case False => True.smt
        case Not(a) => a.smt
        case And(a,b) => Or(Not(a),Not(b)).smt
        case Or(a,b) => And(Not(a),Not(b)).smt
        case _ => smt2op("not", e.smt)
      }
  }
  case class BigOr(es:List[Expr]) extends ArbOp {
    override def factory(es: List[Expr]) = BigOr(es)
    override def smt: String =
      if (es.contains(True)) True.smt
      else if (es.size == 1) es.head.smt
      else if (es.nonEmpty) smt2op("or", es.map(_.smt):_*)
      else False.smt
  }
  case object True extends Constant {
    override def smt: String = "true"
  }
  case object False extends Constant {
    override def smt: String = "false"
  }

  trait Var extends Expr {
    def name:String
    def sort:String
    def shortSort:String = sort.head.toLower.toString
    def replace(f: Var => Var): Var = f(this)
    def smt: String = name
    def vars:Set[Var] = Set(this)
  }
  trait BoolVar extends Var {
    def sort = "Bool"
  }
  trait StringVar extends Var {
    def sort = "String"
  }
  trait IntVar extends Var {
    def sort = "Int"
  }
  trait ArgVar extends Var {
    val n:Int // Nth
    def side:String
    def name: EventID = "arg-"+side+"-"+shortSort+n
    def bind(e: Event): Var = BoundVar(n,e,sort)
  }
  trait LeftArgVar extends ArgVar {
    def side = "l"
  }
  trait RightArgVar extends ArgVar {
    def side = "r"
  }
  case class BoundVar( n:Int, e:Event, sort:String) extends Var {
    override def name: String = e.id+"-"+shortSort+n
  }
  case class BoolArgLeft(n: Int) extends LeftArgVar with BoolVar
  case class BoolArgRight(n: Int) extends RightArgVar with BoolVar
  case class StringArgLeft(n: Int) extends LeftArgVar with StringVar
  case class StringArgRight(n: Int) extends RightArgVar with StringVar
  case class IntArgLeft(n: Int) extends LeftArgVar with IntVar
  case class IntArgRight(n: Int) extends RightArgVar with IntVar

  trait Operation {
    val id: OperationID
  }
  case class Update(id:OperationID) extends Operation
  case class Query(id:OperationID) extends Operation

  case class Event(
      id:EventID,
      txn:TransactionID,
      operationID:OperationID
  ) {

    def commutesWith(other:Event)(implicit g:Graph): Expr = {

      if (g.operationMap(this.operationID).isInstanceOf[Query]
        && g.operationMap(other.operationID).isInstanceOf[Query]) {
        True
      } else if (this.operationID <= other.operationID) {
        g.system.commutativitySpecs(this.operationID,other.operationID).instantiate(this,other)
      } else {
        other.commutesWith(this)
      }
    }

    def absorbedBy(other:Event)(implicit g:Graph): Expr = {
      g.system.absorptionSpecs(this.operationID,other.operationID).instantiate(this,other)
    }

  }

  case class Edge(
      sourceID:EventID,
      targetID:EventID,
      constraint:Expr
  ) {

    @inline
    def source(implicit g:Graph):Event = g.eventMap(sourceID)

    @inline
    def target(implicit g:Graph):Event = g.eventMap(targetID)

  }


}

import ch.ethz.inf.pm.sample.oorepresentation.WeightedGraph
import ch.ethz.inf.pm.td.cloud.BoundedEventGraph._

import scala.util.Random

object BoundedEventGraphEncoder {

  private object EdgeNames {

    def make(label:String,l:TransactionID,r:TransactionID): String =
      "e-"+label+"-"+l+"-"+r

    def isName(a:String): Boolean =
      a.startsWith("e-")

    def deconstruct(str:String):(String,TransactionID,TransactionID) = {
      val Array(a,b,c,d) = str.split("-")
      assert(a=="e")
      (b,c,d)
    }

  }

  private object TransactionNames {

    def make(r:TransactionID): String =
      "txn_"+r

    def isName(a:String): Boolean =
      a.startsWith("txn_")

    def deconstruct(str:String):TransactionID = {
      str.stripPrefix("txn_")
    }

  }

  case class EventGraphRenderer(g:Graph) extends DumpGraph.GraphRenderer[String,String] {
    override def clazz(node: String): String =
      if (TransactionNames.isName(node)) "coral" else "blue"
    override def name(node: String): String = node
    override def label(value: String): String = value
    override def partitioning(value: String): Option[String] =
      g.eventMap.get(value).map(x => TransactionNames.make(x.txn))
  }

  case class Ar(l:TransactionID,r:TransactionID) extends BoolVar {
    override def name: String = EdgeNames.make("ar",l,r)
  }

  case class Ca(l:TransactionID,r:TransactionID) extends BoolVar {
    override def name: String = EdgeNames.make("ca",l,r)
  }

  case class DSG(l:TransactionID,r:TransactionID) extends BoolVar {
    override def name: String = EdgeNames.make("d",l,r)
  }

  case class TDSG(l:TransactionID,r:TransactionID) extends BoolVar {
    override def name: String = EdgeNames.make("td",l,r)
  }

  def findViolations(g: Graph, sessionName:Option[String] = None): Option[WeightedGraph[String,String]] = {

    var violation:Option[WeightedGraph[String,String]] = None

    Z3Prover.withZ3[Unit,Expr,Var] ( { z3 =>

      phi(g,z3)
      z3.check() match {
        case Z3Prover.Sat =>
          val model = z3.extractModel()
          println(model.mkString("\n"))
          val cycle = toGraph(model.filter{x => x._1.startsWith("e-d-")})(g)
          val session = sessionName.getOrElse(Random.alphanumeric.take(5).mkString(""))
          println(DumpGraph("cycle_"+session,cycle,EventGraphRenderer(g)))
          violation = Some(cycle)
        case Z3Prover.Unknown =>
          assert(false)
        case Z3Prover.Unsat =>
          z3.printUnsatCore()
      }

    }, EventGraphZ3Converter )

    violation
  }

  def toGraph(model:Map[String,String])(implicit g:Graph): WeightedGraph[String,String] = {

    val pg = new WeightedGraph.Default[String,String]()

    g.transactions.map(TransactionNames.make).foreach(pg.addNode)
    g.events.foreach(x => pg.addNode(x.id))
    val nodeMap = pg.nodes.zipWithIndex.toMap

    model.foreach {
      case (edgeString, "true") if EdgeNames.isName(edgeString) =>
        val (label,src,tgt) = EdgeNames.deconstruct(edgeString)
        pg.addEdge(nodeMap(TransactionNames.make(src)),nodeMap(TransactionNames.make(tgt)),Some(label))
      case (edgeString, "false") if EdgeNames.isName(edgeString) =>
        ()
      case x =>
        println("skipping "+x)
    }

    pg

  }

  def phi(implicit g:Graph, z3:Z3Prover[Expr,Var]): Unit = {

    ::("==== Axiomatic Model")

    ::("Po in Ca")
    for (e <- g.programOrder)
      assume( Ca(e.source.txn,e.target.txn) )

    ::("Ca implies Ar")
    for (u <- g.transactions; v <- g.transactions if u <= v)
      assume( Implies(Ca(u,v), Ar(u,v)) )

    ::("Transitivity of Ca")
    for (u <- g.transactions; v <- g.transactions; w <- g.transactions if w != u && w != v)
      assume( Implies(And(Ca(u,w), Ca(w,v)), Ca(u,v)) )

    ::("Irreflexivity of Ca")
    for (u <- g.transactions)
      assume( Not(Ca(u,u)) )

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
              Not(u commutesWith v))
            :::
            // Dependencies
            (for (u <- g.updatesByTxn.getOrElse(e,Nil); q <- g.queriesByTxn.getOrElse(f,Nil)) yield
              And(Not(u commutesWith q), Ca(u.txn, q.txn)))
            :::
            // Anti-Dependencies
            (for (q <- g.queriesByTxn.getOrElse(e,Nil); u <- g.updatesByTxn.getOrElse(f,Nil)) yield
              And(Not(u commutesWith q), Not(Ca(u.txn, q.txn))))
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

}

object EventGraphZ3Converter extends Z3Prover.ExpressionConverter[Expr,Var] {
  override def name(v: Var): String = v.name
  override def sort(v: Var): String = v.sort
  override def vars(expr: Expr): Set[Var] = expr.vars
  override def convert(expr: Expr): String = expr.smt
}
