/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.tools

import java.io.{PrintWriter, StringWriter}

import ch.ethz.inf.pm.td.analysis.TouchAnalysisParameters
import ch.ethz.inf.pm.td.compiler.ScriptRetriever
import ch.ethz.inf.pm.td.output.{Exporters, MongoExporter}
import ch.ethz.inf.pm.td.webapi._
import com.typesafe.scalalogging.LazyLogging

/**
  * @author Lucas Brutschy
  */
object Instrumentation extends LazyLogging {

  val LIBRARY_ID = "qvohalzg"
  var freshID = 324

  def fresh = {
    freshID += 1; freshID
  }


  def main(args: Array[String]) = {

    val exporter = new MongoExporter

    var printMode = false
    val newArgs = args filter {

      // Print rather than store to mongo
      case "-print" => printMode = true; false
      case _ => true

    }

    for (a <- newArgs) {
      try {
        println("Instrumenting " + a)
        val ((_, japp), id) = ScriptRetriever.getPath(a)
        val ast = japp.get // Fail if not.
        implicit val cloudDeclarations: Set[Target] = (ast.decls flatMap discoverCloud).toSet
        implicit val scriptID = ScriptID(id)
        val result = (ast.decls map traverse).reduce(_ ++ _) ++ List(makeCloudLibrary(ast))
        if (printMode) {
          println("Done.")
          import com.novus.salat._
          import com.novus.salat.global._
          println((result.results map {
            grater[JResult].toCompactJSON
          }).mkString("\n"))
        } else {
          println("Done.")
          setResult(result.results)
        }
      } catch {
        case x: Throwable =>
          val sw: StringWriter = new StringWriter()
          val pw: PrintWriter = new PrintWriter(sw)
          x.printStackTrace(pw)
          val debugInfo = x.toString + x.getMessage + sw.toString
          if (printMode) {
            println(debugInfo)
          } else {
            exporter.setDebugInformation(debugInfo)
            exporter.setStatus("Failed")
          }
          throw x
      }
    }

  }

  def discoverCloud(x: JDecl): Set[Target] = {

    x match {
      case r: JRecord =>
        if (r.isCloudEnabled || r.isCloudPartiallyEnabled)
          Set(Target("records", r.name + " " + r.category))
        else Set.empty
      case d: JData =>
        if (d.isCloudEnabled)
          Set(Target("data", d.name))
        else Set.empty
      case _ =>
        Set.empty // TODO: add library support
    }

  }

  def traverse(x: JDecl)(implicit cloudDeclarations: Set[Target], scriptID: ScriptID): ResultState = {

    implicit val parentID: String = x.id
    val s = ResultState.empty ++ cloudDeclarations

    x match {
      case a: JAction =>
        implicit val parentField: String = x.id
        a.body.foldLeft(s)(traverse(x.id, "body"))
      case p: JPage =>
        val x1 = p.displayBody.foldLeft(s)(traverse(x.id, "displayBody", inDisplayBody = true))
        val x2 = p.initBody.foldLeft(s)(traverse(x.id, "initBody"))
        x1 ++ x2
      case e: JEvent =>
        e.body.foldLeft(s)(traverse(x.id, "body"))
      case _ =>
        s // TODO: add library support
    }

  }

  def processExprHolder(expr: JExprHolder, parentID: String, parentField: String, insertBeforeID: String, inDisplayBody: Boolean)(inState: ResultState)(implicit scriptID: ScriptID): ResultState = {
    if (!inDisplayBody) {
      val ret = getOps(State.empty ++ inState.tgts, expr.tree)
      val result = if (ret.stmts.isEmpty) Nil else List(JResult(parentID, parentField, insertBeforeID, ret.stmts))
      inState ++ result ++ ret.tgts
    } else inState
  }

  def traverse(parentID: String, parentField: String, inDisplayBody: Boolean = false)(state: ResultState, x: JStmt)(implicit cloudDeclarations: Set[Target], scriptID: ScriptID): ResultState = {

    x match {
      case JExprStmt(id, expr) =>
        processExprHolder(expr, parentID, parentField, id, inDisplayBody)(state)
      case JBoxed(id, body) =>
        body.foldLeft(state)(traverse(id, "body", inDisplayBody))
      case JIf(id, condition, thenBody, elseBody, isElseIf) =>
        val state1 = processExprHolder(condition, parentID, parentField, id, inDisplayBody)(state)
        val state2 = thenBody.foldLeft(state1) {
          traverse(id, "thenBody", inDisplayBody)
        }
        val state3 = elseBody.foldLeft(state2) {
          traverse(id, "elseBody", inDisplayBody)
        }
        state3
      case JWhile(id, condition, body) =>
        // Insert before the loop
        val state1 = processExprHolder(condition, parentID, parentField, id, inDisplayBody)(state)
        // Insert at the end of the loop
        val state2 = processExprHolder(condition, id, "body", "", inDisplayBody)(state1)
        val state3 = body.foldLeft(state2) {
          traverse(id, "body", inDisplayBody)
        }
        state3
      case JFor(id, index, bound, body) =>
        // Bound executed only once
        val state1 = processExprHolder(bound, parentID, parentField, id, inDisplayBody)(state)
        val state2 = body.foldLeft(state1) {
          traverse(id, "body", inDisplayBody)
        }
        state2
      case JForeach(id, iterator, collection, conditions, body) =>
        // Collection is only evaluated once.
        val state1 = processExprHolder(collection, parentID, parentField, id, inDisplayBody)(state)
        // TODO: Handle conditions
        val state2 = body.foldLeft(state1) {
          traverse(id, "body", inDisplayBody)
        }
        state2
      case JInlineActions(id, expr, actions) =>
        var curState = state
        for (action <- actions) {
          action match {
            case JInlineAction(inlineID, _, _, _, body, _, _, _) =>
              curState = body.foldLeft(curState) {
                traverse(inlineID, "body", inDisplayBody = false)
              } // leaving display body
            case JOptionalParameter(_, name, _, subExpr) =>
              curState = processExprHolder(subExpr, parentID, parentField, id, inDisplayBody)(curState)
          }
        }
        curState = processExprHolder(expr, parentID, parentField, id, inDisplayBody)(curState)
        curState
      case _ =>
        state
    }

  }


  def getOps(state: State, x: JExpr)(implicit scriptID: ScriptID): State = {

    // READ ACCESS?
    findTarget(x)(state) match {
      case Some(target) =>
        println(target + "  " + "get")
        state ++ makeInstrumentation(x.id, "get", List(
          ("String", JStringLiteral(x.id + fresh, target.transTarget.toString))
        ))
      case None =>

        x match {
          case c: JCall =>
            if (c.args.nonEmpty) {
              findTarget(c.args.head)(state) match {
                case Some(target) =>
                  val sub = c.args.tail.foldLeft(state)(getOps)
                  println(target + "  " + c.name)
                  sub ++ makeInstrumentation(x.id, c.name, List(
                    ("String", JStringLiteral(x.id + fresh, target.transTarget.toString))
                  ) :: c.args.tail map { y =>
                    ("String", JStringLiteral(x.id + fresh, y.toString))
                  }
                  )
                case None =>
                  c.args.foldLeft(state)(getOps)
              }
            } else {
              c.args.foldLeft(state)(getOps)
            }
          // TODO: Support for local refs?
          case JShow(_, expr) => getOps(state, expr)
          case JReturn(_, expr) => getOps(state, expr)
          case _ => state
        }
    }
  }

  def findTarget(expr: JExpr)(state: State)(implicit scriptID: ScriptID): Option[Target] = {
    expr match {
      case JCall(_, name, _, _, List(JSingletonRef(_, singleton, _)), _, _) if
      state.tgts.contains(Target(singleton, name)) =>
        Some(Target(singleton, name))
      case _ =>
        None
    }
  }

  def makeCloudLibrary(ast: JApp): JResult = {
    val libID = ast.id + fresh
    JResult(
      ast.id,
      "decls",
      ast.decls.last.id,
      List(
        JLibrary(
          id = libID,
          name = "Instrumentation Library",
          libIdentifier = LIBRARY_ID,
          libIsPublished = true,
          scriptName = "Instrumentation Library",
          exportedTypes = "",
          exportedTypeDefs = Nil,
          exportedActions = List(
            JLibAction(
              id = ast.id + fresh,
              name = "Record",
              inParameters = List(
                JLocalDef(
                  id = ast.id + fresh,
                  name = "op",
                  `type` = "Json Builder"
                )
              ),
              outParameters = Nil,
              isPrivate = false,
              isTest = false,
              isQuery = false,
              isAsync = false,
              isOffline = false,
              description = "",
              parentLibId = libID
            )
          ),
          resolveClauses = Nil
        )
      )
    )
  }

  def makeInstrumentation(id: String, opName: String, params: List[(String, JExpr)])(implicit scriptID: ScriptID): List[JStmt] = {
    val opVarName = "operation" + fresh
    val opVarID = id + fresh
    JExprStmt(id + fresh, JExprHolder(id + fresh, None,
      tree = JCall(id + fresh, ":=", "Unknown", None, List(
        JLocalRef(id + fresh, opVarName, opVarID),
        JCall(id + fresh, "create json builder", "Web", None, List(JSingletonRef(id + fresh, "web", "Web")))
      )),
      locals = List(JLocalDef(opVarID, opVarName, "Json Builder"))
    )) ::
      JExprStmt(id + fresh, JExprHolder(id + fresh, None,
        tree = JCall(id + fresh, "set string", "Json Builder", None, List(
          JLocalRef(id + fresh, opVarName, opVarID),
          JStringLiteral(id + fresh, "name"),
          JStringLiteral(id + fresh, opName)
        )), Nil
      )) :: (
      params.zipWithIndex map { x =>
        val ((paramTyp, expr), index) = x
        val method = paramTyp match {
          case "Number" => "set number"
          case "String" => "set string"
          case _ =>
            throw new UnsupportedOperationException("param to cloud operations can only be strings or numbers")
        }
        JExprStmt(id + fresh, JExprHolder(id + fresh, None,
          tree = JCall(id + fresh, method, "Json Builder", None, List(
            JLocalRef(id + fresh, opVarName, opVarID),
            JStringLiteral(id + fresh, "param" + index),
            expr // TODO: Fix ids?
          )), Nil
        ))
      }
      ) ::: List(
      JExprStmt(id + fresh, JExprHolder(id + fresh, None,
        tree = JCall(id + fresh, "Record", "Instrumentation Library", None, List(
          JCall(id + fresh, "Instrumentation Library", "♻", None, List(
            JSingletonRef(id + fresh, "♻", "♻")
          )),
          JLocalRef(id + fresh, opVarName, opVarID)
        )), Nil
      ))
    )
  }

  def setResult(result: Set[JResult]) {

    val settings = TouchAnalysisParameters.get
    import com.mongodb.casbah.Imports._
    import com.novus.salat._
    import com.novus.salat.global._
    val mongoClient = MongoClient(settings.mongoServer, settings.mongoPort)
    val collection = mongoClient(settings.mongoDatabase)("analysisJobs")

    collection.update(MongoDBObject("jobID" -> Exporters.jobID), $set(
      "status" -> "Done",
      "result" -> MongoDBList(result map {
        grater[JResult].asDBObject
      })
    ))

  }

}

case class JResult(parentID: String, parentField: String, insertBeforeID: String, stmt: List[JNode])

case class ScriptID(id: String)

case class Target(typ: String, name: String, origTarget: Option[Target] = None) {
  def transTarget: Target = origTarget.getOrElse(this)

  override def toString = typ + " " + name
}

case class State(stmts: List[JStmt], tgts: Set[Target]) {

  def ++(s: State) = {
    this.copy(stmts = stmts ::: s.stmts, tgts = tgts ++ s.tgts)
  }

  def ++(s: List[JStmt]) = {
    this.copy(stmts = stmts ::: s)
  }

  def ++(s: Set[Target]) = {
    this.copy(tgts = tgts ++ s)
  }

  def ++(s: (List[JStmt], Set[Target])) = {
    this.copy(stmts = stmts ::: s._1, tgts = tgts ++ s._2)
  }

}

object State {

  def empty = State(Nil, Set.empty)

}


case class ResultState(results: Set[JResult], tgts: Set[Target]) {

  def ++(s: ResultState) = {
    this.copy(results = results ++ s.results, tgts = tgts ++ s.tgts)
  }

  def ++(s: List[JResult]) = {
    this.copy(results = results ++ s)
  }

  def ++(s: Set[Target]) = {
    this.copy(tgts = tgts ++ s)
  }

  def ++(s: (List[JResult], Set[Target])) = {
    this.copy(results = results ++ s._1, tgts = tgts ++ s._2)
  }

}

object ResultState {

  def empty = ResultState(Set.empty, Set.empty)

}