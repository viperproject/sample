import ch.ethz.inf.pm.td.analysis.TestRunner

def analyzer(id:String) {
  TestRunner.runIdWithApron(id)
}

TestRunner("aagn",analyzer _)
//TestRunner("aafi",analyzer _)
//TestRunner("aadimryg",analyzer _)
//TestRunner("aaeub",analyzer _)
//TestRunner("aaae",analyzer _)

// Bug 1
// Script avvj, method normalize, with Linear equalities.
// In the inner conditional value>1 in both branches we have that (for instance) i==0, but in the join we obtain a top state...
// TestRunner("avvj",analyzer _) // FIXED

// Bug 5
// Script tglf, method check_for_mines, linear equalities domain.
// We obtain bottom after num=code.tile_top(....)
//TestRunner("tglf",analyzer _)

// Bug 4
// Script mlkc, method square, linear equalities domain.
// We obtain top inside the while loop (but I don't get any alert about loss of precision during the analysis)
//TestRunner("mlkc",analyzer _)

// Bug 7
// Script vrgt, method Run, linear equalities domain.
// Not sure it is a bug: the length of the collection is zero, then when we check the boolean guard of the foreach loop we obtain bottom. Is this result right?
// It is not the correct result. It is a map problem, exactly what we are working on right now.
// TestRunner("vrgt",analyzer _) // FIXED