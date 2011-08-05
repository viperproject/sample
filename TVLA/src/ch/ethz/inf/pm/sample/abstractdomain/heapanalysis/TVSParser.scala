package ch.ethz.inf.pm.sample.abstractdomain.heapanalysis

import scala.util.parsing.combinator._
import scala.io.Source

import Kleene._
import java.io.{FileReader, StringReader, Reader}

/**
 * TVSParser parses the TVS files produced by TVLA.
 */
class TVSParser(val tvp: TVP) extends RegexParsers {

  abstract class Entries(val name: String)
  case class UnaryEntries(n: String, entries: List[(SimpleNode, Kleene)]) extends Entries(n)
  case class BinaryEntries(n: String, entries: List[(SimpleNode, SimpleNode, Kleene)]) extends Entries(n)

  //================================================================================
  // Offered parsing functionality for the output of TVLA
  //================================================================================


  def parseString(source: String): List[TVS[SimpleNode]] = parseReader(new StringReader(source))

  def parseFile(filename: String): List[TVS[SimpleNode]] = {
    val filesource = Source.fromFile(filename)
    parseReader(filesource.bufferedReader)
  }

  def parseReader(in: Reader): List[TVS[SimpleNode]] = {
    parseAll(tvs, in) match {
      case Success(result, _) => return result
      case err: NoSuccess => throw new Exception("Parsing TVS failed")
    }
  }

  def parseResultFile(file: String): Map[String, List[TVS[SimpleNode]]] = {
    parseAll(tvlaResult, new FileReader(file)) match {
      case Success(result, _) => return result
      case err: NoSuccess => throw new Exception("Parsing TVLA result failed")
    }
  }


  //================================================================================
  // Parsers / Combinators
  //================================================================================

  def tvlaResult: Parser[Map[String, List[TVS[SimpleNode]]]] =
    rep(tvlaResultLocation) ^^ {
      Map() ++ _
    }

  def tvlaResultLocation: Parser[(String, List[TVS[SimpleNode]])] = (opt(comment) ~
    "%location") ~> ident ~ (("=" ~ "{") ~> tvs) <~ "}" ^^ {
    case n ~ t => (n, t)
  }

  /* parsers for TVS */
  def tvs: Parser[List[TVS[SimpleNode]]] = (opt(comment) ~> structure) *

  def structure: Parser[TVS[SimpleNode]] = universe ~ predicates ^^ {
    case u ~ p => generateTVS(u, p)
  }

  /**set of nodes (entities) */
  def universe: Parser[List[String]] =
    ("%n" ~ "=" ~ "{") ~> repsep(ident, ",") <~ "}"

  /**unary and binary predicates */
  def predicates: Parser[Map[String, Entries]] =
    ("%p" ~ "=" ~ "{") ~> rep(unary | binary) <~ "}" ^^ (_.map(p => (p.name, p)).toMap)

  def unary: Parser[UnaryEntries] =
    (ident <~ ("=" ~ "{")) ~ repsep(unaryEntry, ",") <~ "}" ^^ {
      case name ~ vals => new UnaryEntries(name, vals)
    }

  def unaryEntry: Parser[(SimpleNode, Kleene)] =
    ident ~ opt(":" ~> value) ^^ {
      case n ~ Some(v) => (SimpleNode(n), v)
      case n ~ None => (SimpleNode(n), True)
    }

  def binary: Parser[BinaryEntries] =
    (ident <~ ("=" ~ "{")) ~ repsep(binaryEntry, ",") <~ "}" ^^ {
      case name ~ vals => new BinaryEntries(name, vals)
    }

  def binaryEntry: Parser[(SimpleNode, SimpleNode, Kleene)] =
    (ident <~ "->") ~ ident ~ opt(":" ~> value) ^^ {
      case n1 ~ n2 ~ Some(v) => (SimpleNode(n1), SimpleNode(n2), v)
      case n1 ~ n2 ~ None => (SimpleNode(n1), SimpleNode(n2), True)
    }

  def value: Parser[Kleene] = (
    "0" ^^ (x => False)
      | "1/2" ^^ (x => Unknown)
      | "1" ^^ (x => True)
    )

  /**identifiers for nodes (entities) and predicates,
        may look like <id> or "<id>[<id>,...,<id>] */
  def ident: Parser[String] =
    """\w+(\[\w+(,\w+)*\])?""".r

  /**allow C and C++ style comments (not nested) */
  def comment: Parser[String] =
    """(//.*)|(/\*([^/]|[^*]/)*\*/)""".r


  //================================================================================
  // Transformation to more a specific representation using the information
  // from the predicate declarations
  //================================================================================

  /**
   * Generate an instance of our TVS class. We use the information which predicates
   * of what types were declared, since this is NOT encoded in a TVS.
   */
  def generateTVS(universe: List[String], predicates: Map[String, Entries]): TVS[SimpleNode] = {
    val result = new TVS[SimpleNode]
    result.nodes = universe map {
      SimpleNode(_)
     }

    for (p <- tvp.programVariables) {
      val newvar: ProgramVariablePredicate[SimpleNode] =
      predicates.get(p) match {
        case Some(UnaryEntries(_, entries)) =>
          assert(entries.isEmpty || entries.size == 1, "TVS: Unique Predicate "+p+" contains more than one entry")
          if (entries.isEmpty)
            new ProgramVariablePredicate(p, None)
          else
            new ProgramVariablePredicate(p, Some(entries.head._1))
        case _ =>
          new ProgramVariablePredicate(p, None)
      }

      result.programVariables += p -> newvar
    }

    for (p <- tvp.names) {
      predicates.get(p) match {
        case Some(UnaryEntries(_,entries)) =>
          result.names += p -> new NamePredicate(p, entries.toMap)
        case _ =>
          result.names += p -> new NamePredicate(p, Map[SimpleNode,Kleene]())
      }
    }

    for (p <- tvp.fields) {
      predicates.get(p) match {
        case Some(BinaryEntries(_,entries)) =>
          val f = entries map { case (l,r,t) => (l,(r,t))}
          result.fields += p -> new FieldPredicate(p, f.toMap)
        case _ =>
          result.fields += p -> new FieldPredicate(p, Map())
      }
    }

    for (p <- tvp.transitiveReachability) {
      predicates.get(p) match {
        case Some(BinaryEntries(_,entries)) =>
          val f = entries map { case (l,r,t) => ((l,r),t)}
          result.transitiveReachability += p -> new BinaryPredicate(p, f.toMap)
        case _ =>
          result.transitiveReachability += p -> new BinaryPredicate(p, Map())
      }
    }

    for (p <- tvp.sharing) {
      predicates.get(p) match {
        case Some(UnaryEntries(_,entries)) =>
          result.sharing += p -> new UnaryPredicate(p, entries.toMap)
        case _ =>
          result.sharing += p -> new UnaryPredicate(p, Map[SimpleNode,Kleene]())
      }
    }

    for (p <- tvp.variableReachability) {
      predicates.get(p) match {
        case Some(UnaryEntries(_,entries)) =>
          result.variableReachability += p -> new UnaryPredicate(p, entries.toMap)
        case _ =>
          result.variableReachability += p -> new UnaryPredicate(p, Map[SimpleNode,Kleene]())
      }
    }

    predicates.get("sm") match {
        case Some(UnaryEntries(p,entries)) =>
          result.summarization = new UnaryPredicate(p, entries.toMap)
        case _ =>
          result.summarization = new UnaryPredicate("sm", Map[SimpleNode,Kleene]())
    }

    result
  }
}
