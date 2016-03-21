package ch.ethz.inf.pm.sample.analysis



//class SiliconWithPermissionInference(private var debugInfo: Seq[(String, Any)] = Nil) extends Silicon {
//
//}

//class SiliconWithInference(private var debugInfo: Seq[(String, Any)] = Nil)
//  extends Silicon {
//
//  // For verification errors, the specification inference is to blame.
//  // Override the verifier name such that one can use `UnexpectedError`
//  // annotations etc. and refer to issues in the Sample issue tracker.
//  override val name: String = "sample"
//
//  /** Extend the given program with inferred specifications and verify it. */
//  override def verify(program: sil.Program) = {
//    val runner = PredicateAnalysisRunner
//    val results = runner.run(program)
//
//    val programExtender = ProgramExtender[ApronInterface.Default]()
//    val extendedProgram = programExtender.extend(program, results)
//
//    assert(isWellFormed(extendedProgram),
//      "the extended program is not well-formed")
//
//    // Silicon sets the log level for logger of the current package to "OFF"
//    // unless specified otherwise using a command line argument.
//    // Since we're not interested in Silicon log messages anyway, that's okay.
//    // However, the original log level should be restored again later.
//    // TODO: Find a more elegant solution.
//    val result = super.verify(extendedProgram)
//
//    result
//  }
//
//  /** Returns whether the given program can be parsed and type-checked.
//    *
//    * Invalid changes to a program are not always caught by the SIL AST.
//    */
//  def isWellFormed(program: sil.Program): Boolean = {
//    val tempFile = File.makeTemp()
//    tempFile.writeAll(program.toString())
//
//    val frontend = new DummySilFrontend
//    frontend.init(DummyVerifier)
//
//    // TODO: Find a better way of turning the File into a Path
//    frontend.reset(Seq(Paths.get(tempFile.toString())))
//
//    frontend.run() == Success
//  }
//}
