package ch.ethz.inf.pm.td.tools

import ch.ethz.inf.pm.sample.abstractdomain.{Lattice, SetDomain}
import ch.ethz.inf.pm.td.compiler.ScriptRetriever
import ch.ethz.inf.pm.td.parser._
import ch.ethz.inf.pm.td.transform.{LoopRewriter, Matcher}

import scala.io.Source

/**
 * Finds all scripts using a specific construct
 */
object FindCloud {

  def main(args:Array[String]) {

    var i = 0
    for (arg <- args) {

      val files = if (arg.endsWith(".txt")) {
        Source.fromFile(arg, "utf-8").getLines()
      } else {
        List(arg)
      }

      for (file <- files) {
        i = i + 1
        var ((script,japp), id) = ScriptRetriever.getPath(file)

        var name = ""
        japp match {
          case None => ()
          case Some(x) =>
            name = x.name
        }

        println(i+") =================== Retrieved " + id + " named "+name)

        script = LoopRewriter(script)
        var set:Set[String] = Set.empty

        Lattice.lfp(SetDomain.Default.Bottom[Expression](), { state: SetDomain.Default[Expression] =>
          var retState = state
          Matcher(script)({
            case TableDefinition(idx, typ, _, _, true, _, _, _) =>
              retState = retState + Access(SingletonReference("records", "records"), Identifier(idx + " " + typ), Nil)
//            case VariableDefinition(Parameter(idx,typ),flags) if flags.contains("cloudenabled") =>
//              retState = retState + Access(SingletonReference("data", "data"), Identifier(idx), Nil)
            case _ =>
              ()
          }, {
            case ExpressionStatement(expr) =>
              var matching = false
              Matcher(expr)({ x: Expression =>
                if (retState.contains(x)) {
                  matching = true
                }
              })
              if (matching) {
                expr match {
                  case Access(subj, Identifier(":="), a) =>
                    retState = retState + subj
                  case _ => ()
                }
                set = set + ("   "+PrettyPrinter(expr)({ (pp: IdPositional, s: String) => s }))
              }
            case _ => ()
          }, { _ => })
          retState
        }, 10)

        println(set.toList.sorted.mkString("\n"))
      }

    }
  }

}
