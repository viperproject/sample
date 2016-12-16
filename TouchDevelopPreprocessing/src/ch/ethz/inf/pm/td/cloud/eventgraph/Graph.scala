/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package ch.ethz.inf.pm.td.cloud.eventgraph

import ch.ethz.inf.pm.sample.oorepresentation.LabeledGraph
import ch.ethz.inf.pm.td.cloud.eventgraph.Encoder.EventArgumentNames
import com.novus.salat.annotations.Salat
import com.novus.salat.{TypeHintStrategy, _}

object Graph {

  type OperationID = String
  type TransactionID = String
  type EventID = String

  def fromJSON(a: String): Graph = {
    grater[Graph].fromJSON(a)
  }

  implicit val ctx = new Context {
    val name = "WithoutPackage"
    override val typeHintStrategy: TypeHintStrategy = new TypeHintStrategy {
      override val when = TypeHintFrequency.WhenNecessary
      override val typeHint: String = "_t"
      lazy val pkg: String = this.getClass.getPackage.getName + "."

      def decode(in: Any): String = in match {
        case s: String if s != null => pkg + s
        case x => throw new Error("Can't encode supplied value '%s'".format(x))
      }

      def encode(in: String): String = {
        assert(in.startsWith(pkg))
        in.stripPrefix(pkg)
      }
    }
  }

}

import ch.ethz.inf.pm.td.cloud.eventgraph.Graph._

case class Graph(
    events: List[Event],
    programOrder: List[Edge],
    system: SystemSpecification
) {

  lazy val eventMap: Map[String, Event] = events.map { x => (x.id, x) }.toMap
  lazy val operationMap: Map[String, Operation] = system.operations.map { x => (x.id, x) }.toMap
  lazy val updates: List[Event] = events.filter(x => operationMap(x.operationID).isInstanceOf[Update])
  lazy val queries: List[Event] = events.filter(x => operationMap(x.operationID).isInstanceOf[Query])
  lazy val transactions: List[TransactionID] = events.map(_.txn).distinct
  lazy val updatesByTxn: Map[TransactionID, List[Event]] = updates.groupBy(_.txn)
  lazy val queriesByTxn: Map[TransactionID, List[Event]] = queries.groupBy(_.txn)

  def toJSON: String = {
    grater[Graph].toPrettyJSON(this)
  }

  def toLabeledGraph: LabeledGraph[String, String] = {

    val pg = new LabeledGraph[String, String]()

    transactions.foreach {
      x =>
        val node = TransactionNames.make(x)
        pg.addNode(node)
        pg.setNodeLabel(node, x.toString)
        pg.setNodeClass(node, "coral")
    }

    events.foreach {
      x =>
        val node = x.id
        pg.addNode(node)
        pg.setNodeLabel(node, node + " (" + pg.getNodeLabel(node) + ")")
        eventMap.get(node).foreach {
          x =>
            val txn = TransactionNames.make(x.txn)
            pg.addPartitioning(node, txn)
        }
        pg.setNodeClass(node, "blue")
    }

    pg
  }

}

object TransactionNames {

  def make(r: TransactionID): String =
    "txn_" + r

  def isName(a: String): Boolean =
    a.startsWith("txn_")

  def deconstruct(str: String): TransactionID = {
    str.stripPrefix("txn_")
  }

}

@Salat
trait Expr {

  def instantiate(l: Event, r: Event): Expr =
    replace({
      case a: LeftArgVar => a.bind(l)
      case a: RightArgVar => a.bind(r)
      case x => x
    })

  def replace(f: Var => Var): Expr

  def vars: Set[Var]

  def smt: String

  def smt2op(a: String, ops: String*): String = "(" + a + " " + ops.mkString(" ") + ")"

}

@Salat
trait UnOp extends Expr {
  val e: Expr

  def factory(e: Expr): Expr

  override def replace(f: (Var) => Var): Expr = factory(e.replace(f))

  override def vars: Set[Var] = e.vars
}

@Salat
trait BinOpExpr extends Expr {
  val l: Expr
  val r: Expr

  def factory(l: Expr, r: Expr): Expr

  override def replace(f: (Var) => Var): Expr = factory(l.replace(f), r.replace(f))

  override def vars: Set[Var] = l.vars ++ r.vars
}

@Salat
trait ArbOp extends Expr {
  val es: List[Expr]

  def factory(es: List[Expr]): Expr

  override def replace(f: (Var) => Var): Expr = factory(es map {
    _.replace(f)
  })

  override def vars: Set[Var] = if (es.isEmpty) Set.empty else es.map(_.vars).reduce(_ ++ _)
}

@Salat
trait Constant extends Expr {
  override def replace(f: (Var) => Var): Expr = this

  override def vars: Set[Var] = Set.empty
}

case class Implies(l: Expr, r: Expr) extends BinOpExpr {
  override def factory(l: Expr, r: Expr) = Implies(l, r)

  override def smt: String = smt2op("=>", l.smt, r.smt)
}

case class Iff(l: Expr, r: Expr) extends BinOpExpr {
  override def factory(l: Expr, r: Expr) = Iff(l, r)

  override def smt: String = smt2op("=", l.smt, r.smt)
}

case class And(l: Expr, r: Expr) extends BinOpExpr {
  override def factory(l: Expr, r: Expr) = And(l, r)

  override def smt: String = smt2op("and", l.smt, r.smt)
}

case class Or(l: Expr, r: Expr) extends BinOpExpr {
  override def factory(l: Expr, r: Expr) = Or(l, r)

  override def smt: String = smt2op("or", l.smt, r.smt)
}

case class Equal(l: Expr, r: Expr) extends BinOpExpr {
  override def factory(l: Expr, r: Expr) = Equal(l, r)

  override def smt: String = smt2op("=", l.smt, r.smt)
}

case class Unequal(l: Expr, r: Expr) extends BinOpExpr {
  override def factory(l: Expr, r: Expr) = Unequal(l, r)

  override def smt: String = smt2op("not", smt2op("=", l.smt, r.smt))
}

case class Not(e: Expr) extends UnOp {
  override def factory(e: Expr) = Not(e)

  override def smt: String =
    e match {
      case True => False.smt
      case False => True.smt
      case Not(a) => a.smt
      case And(a, b) => Or(Not(a), Not(b)).smt
      case Or(a, b) => And(Not(a), Not(b)).smt
      case _ => smt2op("not", e.smt)
    }
}

case class BigOr(es: List[Expr]) extends ArbOp {
  override def factory(es: List[Expr]) = BigOr(es)

  override def smt: String =
    if (es.contains(True)) True.smt
    else if (es.size == 1) es.head.smt
    else if (es.nonEmpty) smt2op("or", es.map(_.smt): _*)
    else False.smt
}

case class BigAnd(es: List[Expr]) extends ArbOp {
  override def factory(es: List[Expr]) = BigAnd(es)

  override def smt: String =
    if (es.contains(False)) False.smt
    else if (es.size == 1) es.head.smt
    else if (es.nonEmpty) smt2op("and", es.map(_.smt): _*)
    else True.smt
}

case object True extends Constant {
  override def smt: String = "true"
}

case object False extends Constant {
  override def smt: String = "false"
}

@Salat
trait Var extends Expr {
  def name: String

  def sort: String

  def shortSort: String = sort.head.toLower.toString

  def replace(f: Var => Var): Var = f(this)

  def smt: String = name

  def vars: Set[Var] = Set(this)
}

@Salat
trait BoolVar extends Var {
  def sort = "Bool"
}

@Salat
trait StringVar extends Var {
  def sort = "String"
}

@Salat
trait IntVar extends Var {
  def sort = "Int"
}

@Salat
trait ArgVar extends Var {
  val n: Int

  // Nth
  def side: String

  def name: EventID = "arg-" + side + "-" + shortSort + n

  def bind(e: Event): Var = BoundVar(n, e, sort)
}

@Salat
trait LeftArgVar extends ArgVar {
  def side = "l"
}

@Salat
trait RightArgVar extends ArgVar {
  def side = "r"
}

case class BoundVar(n: Int, e: Event, sort: String) extends Var {
  override def name: String = EventArgumentNames.make(e.id, shortSort, n)
}

case class BoolArgLeft(n: Int) extends LeftArgVar with BoolVar

case class BoolArgRight(n: Int) extends RightArgVar with BoolVar

case class StringArgLeft(n: Int) extends LeftArgVar with StringVar

case class StringArgRight(n: Int) extends RightArgVar with StringVar

case class IntArgLeft(n: Int) extends LeftArgVar with IntVar

case class IntArgRight(n: Int) extends RightArgVar with IntVar

@Salat
trait Operation {
  val id: OperationID
}

case class Update(id: OperationID) extends Operation

case class Query(id: OperationID) extends Operation

case class Event(
    id: EventID,
    txn: TransactionID,
    operationID: OperationID
) {

  def commutesWith(other: Event)(implicit g: Graph): Expr = {

    if (g.operationMap(this.operationID).isInstanceOf[Query]
      && g.operationMap(other.operationID).isInstanceOf[Query]) {
      True
    } else if (this.operationID <= other.operationID) {
      g.system.commutativitySpecs(this.operationID, other.operationID).instantiate(this, other)
    } else {
      other.commutesWith(this)
    }
  }

  def absorbedBy(other: Event)(implicit g: Graph): Expr = {
    g.system.absorptionSpecs(this.operationID, other.operationID).instantiate(this, other)
  }

}

case class Edge(
    sourceID: EventID,
    targetID: EventID,
    constraint: Expr
) {

  @inline
  def source(implicit g: Graph): Event = g.eventMap(sourceID)

  @inline
  def target(implicit g: Graph): Event = g.eventMap(targetID)

}