package ch.ethz.inf.pm.sample.oorepresentation

case class MethodPosition(mdecl: MethodDeclaration, cfgPos: CFGPosition)

object StatementCollector {
  type Collector[R] = Statement => MethodPosition => Option[R]

  def collectClassStmts[R](f: Collector[R], clazz: ClassDefinition): List[R] = {
    clazz.methods flatMap { mdecl => collectMethodStmts(f, mdecl) }
  }

  def collectMethodStmts[R](f: Collector[R], mdecl: MethodDeclaration): List[R]  = {

    def collectStmt(stmt: Statement, programLoc: MethodPosition): List[R] = {
      val subs =
        stmt match {
          case Assignment(_, left, right) =>
            collectStmt(left, programLoc) ++  collectStmt(right, programLoc)
          case VariableDeclaration(_, _, _, Some(declStmt)) => collectStmt(declStmt, programLoc)
          case access: FieldAccess => collectStmt(access.obj, programLoc)
          case methodCall: MethodCall =>
            collectStmt(methodCall.method, programLoc) ++ (methodCall.parameters flatMap { p => collectStmt(p, programLoc) })
          case Throw(_, exprStmt) => collectStmt(exprStmt, programLoc)
          case _ => Nil
        }
      f(stmt)(programLoc).toList ++ subs
    }


    val cfg = mdecl.body

    cfg.nodes.zipWithIndex flatMap { case (block, blockIdx) =>
      block.zipWithIndex flatMap { case (stmt, stmtIdx) =>
        val programLoc = MethodPosition(mdecl, CFGPosition(blockIdx, stmtIdx))
        collectStmt(stmt, programLoc)
      }
    }
  }


}
