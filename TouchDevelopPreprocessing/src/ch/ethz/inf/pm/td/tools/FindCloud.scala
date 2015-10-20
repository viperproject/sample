package ch.ethz.inf.pm.td.tools

import ch.ethz.inf.pm.sample.abstractdomain.{Lattice, SetDomain}
import ch.ethz.inf.pm.td.analysis.TouchAnalysisParameters
import ch.ethz.inf.pm.td.compiler.ScriptRetriever
import ch.ethz.inf.pm.td.parser._

import scala.collection.mutable
import scala.io.Source

/**
 * Finds all scripts using a specific construct
 */
object FindCloud {

  val HARD_PRUNE = true // prune hard.

  // Interesting: kjxzcgcv
  // Interesting: kmac
  // Interesting: gmxr
  // A bit intere

  val excludedScripts = Set(
      ""
//    "cpmjwmwm", // Broken
//    "ibtjkefl", // Broken
//    "ctbgc", // Broken
//    "mfixc", // Broken
//    "aeklg", // Nothing
//    "avfrc", // Nothing
//    "kmac", // Does make sense to use cloud
//    "oaoja", //  Does make sense to use cloud
//    "qjcbmkie", // Duplicate of sxseehfr
//    "ogccg", // Does not reference the clodu
//    "afbd",  // Just one of the maps
//    "wiwxa", // Just one of the maps
//    "akyuo", // Just one of the maps
//    "ynoqc", // Just one of the maps
//    "mwgcc", // Just one of the maps
//    "grrvc", // Just one of the maps
//    "ylhcc", // Just one of the counters - its fine
//    "acayip", // Just one of the counters - its fine
//    "celr", // Just one of the counters - its fine
//    "gmoe", // Just one of the counters - its fine
//    "kwcc", // Just one of the counters - its fine
//    "pgksa", // Just one of the counters - its fine
//    "urigi", // Just one of the counters - its fine
//    "kxmfa", // Just one of the counters - its fine
//    "azuuc", // Just one of the counters - its fine
//    "mudz", // Just one of the counters - its fine
//    "gvdkc", // Just one of the counters - its fine
//    "iueq", // Just one of the counters - its fine
//    "suwhg", // Just one of the counters - its fine
//    "iyju", // Just one of the counters - its fine
//    "yevee", // Just one of the counters - its fine
//    "sneva", // Just one of the cloud pics
//    "cclle", // Just one of the cloud pics
//    "ssevc", // Just one of the cloud pics
//    "ckiqg", // Just one of the cloud pics
//    "spyxa", // Just one of the cloud pics
//    "mayyg", // Just one of the cloud pics
//    "qhimb", // Just one of the cloud pics
//    "ecsic", // Just one of the favorite numbers
//    "cjicc", // Just one of the voters
//    "wbova", // Number voter, but seems completely fine
//    "ikpvc", // Does not make much sense to use cloud types like that - Flying Rocks game
//    "gdbo",  // Picture diary, uses cloud for backup only - not interesting
//    "snuy",  // SAME as gdbo
//    "klwf",  // Interesting, but partly broken!
//    "gmxr",  // Interesting, but multi-player mode broken!
//    "mogbb", // Okay interesting, but does not really make sense to use cloud types here!
//    "ohgxa", // Okay interesting -- game seems broken, advertisements seem top be fine
//    "wbuei", // Okay interesting, but can't interfere with each other
//    "ulvma", // Okay interesting, but can't interfere with each other
//    "uilfc", // SAME AS kzwue
//    "wxwsc", // SAME AS fqaba
//    "ecsng", // SAME as qzju
//    "okdcc", // Allows arbitrary modification of a row (through user). Also, a shit script
//    "kzwue", // Just one of the counters - it contains a bug (which is robust)
//    "uvjba", // A bit interesting: Contains a concurrency bug (which is robust) -- It is a proper application and short!
//    "gcane", // A bit interesting: Contains a concurrency bug (which is robust) -- It is a proper application and short! (same as above)
//    //  "cvuz",  // Could be very interesting, chat application, did not find anything
//    "gbtxe", // Could be very interesting, social network
//    "whpgc", // Could be very interesting, vulcanization calc
//    "qnpge", // Could be very interesting, metaverse
//    "qhqec", // Could be VERY interesting - Expense Tracker
//    "kqfnc", // Could be VERY interesting - HackER - clearly not robust. Not supposed to be used in the cloud
//    "fqaba", // Could be very interesting, Chatter Box (I could not see non-robust behavior)
//    "qwidc", // Contains the sorting code. Pretty sure this is not robust, or is it?
//    "pyhy",  // Broken due to TD bug
//    "cavke", // Could be interesting, did not find anything (TouchDatabase)
//    "qzju"   // Could be interesting, did not find anything (cloud cards)
  )

  val fromThePaper = Set(
    "ruef",  // Lets users enter their qualities (either from a list, or
             //   freely entered) and finds and displays other users that share them.
    "spji",  // A chat application.
    "vkrpa", // Program a tiny robot using a simple language,
             //   then share your scripts with other users.
    "nggfa", // An app for quickly polling an audience and displaying
             //   the responses as a grid of colors.
    "nvoha", // Allows easy recording of expenses in a table.
    "etww",  // Used to determine the winner of the “Touch of
             //   Summer” coding contest.
    "blqz",  // A general-purpose list that can be concurrently edited.
    "nvjh",  // A library for matching multiple players
             //   to play games together.
    "sxjua"  // A simple rock/paper/scissors game that
             //   uses the cloud game selector library
  )


  val excludedUsers = Set(
      ""
//    "pboj", // TD Samples
//    "jeiv", // TD Docs
//    "frsk", // TD Demo
//    "vnzw", // TD Tests. ret
//    "wonm", // Michal
//    "ajlk", // Peli
//    "gxfb", // Nikolai
//    "ikyp", // Tom
//    "bqsl", // Sebastian
//    "dlkr", // Manuel
//    "expza", // Jonathan
//    "lxxx" // Microsoft Virtual Academy
  )

  def main(args:Array[String]) {

    var i = 0
    for (arg <- args) {

      val files = if (arg.endsWith(".txt")) {
        Source.fromFile(arg, "utf-8").getLines()
      } else if (arg == "-paperExamples") {
        fromThePaper.map("td://"+_)
      } else if (arg == "-readFromDB") {

        val settings = TouchAnalysisParameters.get
        import com.mongodb.casbah.Imports._
        val mongoClient = MongoClient(settings.mongoServer, settings.mongoPort)
        val collection = mongoClient(settings.mongoDatabase)("programs")
        (for (doc <- collection.find("ast.decls" $elemMatch MongoDBObject("isCloudEnabled" -> true ), MongoDBObject("programID" -> 1, "script.userid" -> 1, "script.updateid" -> 1, "script.ishidden" -> 1 ))) yield {
          val programID = doc.getAsOrElse[String]("programID","xxxxxxxxxxx")
          val user = doc.expand[String]("script.userid").getOrElse[String]("")
          val update = doc.expand[String]("script.updateid").getOrElse[String]("")
          val ishidden = doc.expand[Boolean]("script.ishidden").getOrElse[Boolean](true)
          if (programID.length > 10) {
            println("skipping " + programID + " (temporary identifier)")
            None
          } else if (excludedScripts.contains(programID)) {
            println("skipping " + programID + " (manually excluded)")
            None
          } else if (excludedScripts.contains(programID)) {
            println("skipping " + programID + " (manually excluded)")
            None
          } else if (update != programID) {
            println("skipping "+programID+", as it is outdated. update: "+update)
            None
          } else if (ishidden) {
            println("skipping " + programID + " (ishidden)")
            None
          } else if (excludedUsers.contains(user)) {
            println("skipping "+programID+" (by Microsoft employee)")
            None
          } else {
            Some("td://"+programID)
          }
        }).flatten.toList

      } else {
        List(arg)
      }

      println("Will look at "+files.size+" scripts")

      for (file <- files) {
        i = i + 1
        val ((script,japp), id) = ScriptRetriever.getPath(file)

        var name = ""
        japp match {
          case None => ()
          case Some(x) =>
            name = x.name
        }

        println(i+") =================== Retrieved " + id + " named "+name)

        val set:mutable.Set[String] = mutable.Set.empty
        Lattice.lfp(SetDomain.Default.Bottom[(Expression,Artifact)](), { state: SetDomain.Default[(Expression,Artifact)] =>
          val x = Transform(state,script,set)
          x.retState
        }, 10)

        println(set.toList.sorted.mkString("\n"))
      }

    }
  }

  case class Transform(state:SetDomain.Default[(Expression,Artifact)],script:Script,set:mutable.Set[String]) {
    var retState = state
    var matching:Set[Artifact] = Set.empty[Artifact]
    var insideDisplayCode = false

    def visitDecl(scr:Script) {
      visitDecl(scr.declarations)
    }

    def visitDecl(decls:List[Declaration]) {
      decls foreach visitDecl
    }

    def visitDecl(decl:Declaration) {
      decl match {
        case TableDefinition(idx, typ, _, _, true, _, _, _) =>
          retState = retState + (Access(SingletonReference("records", "records"), Identifier(idx + " " + typ), Nil),Record(idx + " " + typ))
        case VariableDefinition(Parameter(idx,typ),flags) if flags.contains("cloudenabled") && flags.get("cloudenabled").get == Left(true) =>
          retState = retState + (Access(SingletonReference("data", "data"), Identifier(idx), Nil),Data(idx))
        case ActionDefinition(_,_,_,bd,_,_) => bd.foreach(visitStmt)
        case PageDefinition(_,_,_,initBody,displayBody,_,_) =>
          initBody.foreach(visitStmt)
          insideDisplayCode = true
          displayBody.foreach(visitStmt)
          insideDisplayCode = false
        case _ => ()
      }
    }

    def visitStmt(stmts:List[Statement]) {
      stmts foreach visitStmt
    }

    def visitStmt(stmt:Statement) {
      // visit expressions
      if (!insideDisplayCode) {
        stmt match {
          case For(loc, up, body) =>
            matching = Set.empty
            visitExpr(up)
            for (m <- matching) {
              set += ("   for (" + PrettyPrinter(up)({ (pp: IdPositional, s: String) => s })+") {...}" + "// referencing " + m)
            }
          case While(cond, body) =>
            matching = Set.empty
            visitExpr(cond)
            for (m <- matching) set += ("   while (" + PrettyPrinter(cond)({ (pp: IdPositional, s: String) => s })+") {...}" + "// referencing " + m)
          case Foreach(loc, coll, guards, body) =>
            matching = Set.empty
            visitExpr(coll)
            for (m <- matching) {
              set += ("   foreach (" + PrettyPrinter(coll)({ (pp: IdPositional, s: String) => s })+") {...}" + "// referencing " + m)
              retState = retState + (LocalReference(loc),m)
            }
            guards foreach {
              x =>
                matching = Set.empty
                visitExpr(x)
                for (m <- matching) set += ("   if (" + PrettyPrinter(x)({ (pp: IdPositional, s: String) => s })+") {...}" + "// referencing " + m)
            }
          case If(cond, then, els) =>
            matching = Set.empty
            visitExpr(cond)
            for (m <- matching) set += ("   if (" + PrettyPrinter(cond)({ (pp: IdPositional, s: String) => s })+") {...}" + "// referencing " + m)
          case WhereStatement(expr, handlers, optParam) =>
            matching = Set.empty
            visitExpr(expr)
            for (m <- matching) set += ("   " + PrettyPrinter(expr)({ (pp: IdPositional, s: String) => s })+") where {...}" + "// referencing " + m)
            optParam foreach {
              x =>
                matching = Set.empty
              visitExpr(x)
              for (m <- matching) set += "   optional parameter match" + "// referencing " + m
            }
          case ExpressionStatement(e@Access(subj, Identifier(":="), a)) =>
            matching = Set.empty
            visitExpr(e)
            for (m <- matching) {
              val entry = ("   " + PrettyPrinter(stmt)({ (pp: IdPositional, s: String) => s })) + "// referencing " + m
              if (!HARD_PRUNE || entry.contains("at") || entry.contains("add")) {
                retState = retState + (subj,m)
              }
              set += entry
            }
          case ExpressionStatement(expr) =>
            matching = Set.empty
            visitExpr(expr)
            for (m <- matching) set += ("   " + PrettyPrinter(expr)({ (pp: IdPositional, s: String) => s })) + "// referencing " + m
          case Skip() => ()
          case Break() => ()
          case Continue() => ()
          case Box(_) => ()
          case Show(expr) =>
            matching = Set.empty
            visitExpr(expr)
            for (m <- matching) set += ("   show" + PrettyPrinter(expr)({ (pp: IdPositional, s: String) => s })) + "// referencing " + m
          case Return(expr) =>
            matching = Set.empty
            visitExpr(expr)
            for (m <- matching) set += ("   return" + PrettyPrinter(expr)({ (pp: IdPositional, s: String) => s })) + "// referencing " + m
        }
      }

      // visit statements
      stmt match {
        case For(loc,up,body) => visitStmt(body)
        case While(cond,body) => visitStmt(body)
        case Foreach(loc,coll,guards,body) => visitStmt(body)
        case If(cond,then,els) => visitStmt(then); visitStmt(els)
        case Box(body) => visitStmt(body)
        case WhereStatement(expr,handlers,optParam) =>
          val tmp = insideDisplayCode
          insideDisplayCode = false
          handlers foreach visitStmt
          insideDisplayCode = tmp
        case _ => ()
      }
    }

    def visitExpr(exprs:List[Expression]) {
      exprs foreach visitExpr
    }

    def visitExpr(expr:Expression) {
      val x = retState.toSet(Set.empty).find{_._1 == expr}
      if (x.isDefined) {
        matching = matching + x.get._2
      }
      expr match {
        case Access(subj,_,args) => visitExpr(subj); visitExpr(args)
        case _ => ()
      }
    }

    def visitStmt(handler:InlineAction) {
      visitStmt(handler.body)
    }

    def visitExpr(opt:OptionalParameter) {
      visitExpr(opt.expr)
    }

    visitDecl(script)

  }

  trait Artifact
  case class Data(str:String) extends Artifact
  case class Record(str:String) extends Artifact
  case class RecordItem(str:String) extends Artifact


}
