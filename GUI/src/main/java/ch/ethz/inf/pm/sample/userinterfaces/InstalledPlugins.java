package ch.ethz.inf.pm.sample.userinterfaces;

import ch.ethz.inf.pm.sample.abstractdomain.HeapDomain;
import ch.ethz.inf.pm.sample.abstractdomain.SemanticAnalysis;
import ch.ethz.inf.pm.sample.abstractdomain.accesspermissions.AccessPermissionsAnalysis;
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis.*;
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.ApronAnalysis;
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.NonRelationalNumericalAnalysis;
import ch.ethz.inf.pm.sample.oorepresentation.scalalang.ScalaCompiler;
import ch.ethz.inf.pm.sample.td.cost.loops.CostAnalysis;
import ch.ethz.inf.pm.sample.td.cost.loops.LoopCostCompiler;
import ch.ethz.inf.pm.td.analysis.TouchAnalysis;
import ch.ethz.inf.pm.td.compiler.TouchCompiler;
import ch.ethz.inf.pm.sample.abstractdomain.stringdomain.stringanalysis.BricksAnalysis;
import ch.ethz.inf.pm.sample.abstractdomain.stringdomain.stringanalysis.PrefixAndSuffixAnalysis;
import ch.ethz.inf.pm.sample.abstractdomain.stringdomain.stringanalysis.SurelyAndMaybeContainedCharactersAnalysis;

public class InstalledPlugins {
    public static ch.ethz.inf.pm.sample.oorepresentation.Compiler[] compilers;
    public static SemanticAnalysis<?>[] analyses;
    public static HeapDomain<?, ?>[] heapanalyses;

    static  {
        compilers = new ch.ethz.inf.pm.sample.oorepresentation.Compiler[] {
                new ScalaCompiler(),
                new TouchCompiler(),
                new LoopCostCompiler()
        };

        analyses = new SemanticAnalysis<?>[] {
                new NonRelationalNumericalAnalysis(),
                new AccessPermissionsAnalysis(),
                new ApronAnalysis(),
                new PrefixAndSuffixAnalysis(),
                new BricksAnalysis(),
                new SurelyAndMaybeContainedCharactersAnalysis(),
                new TouchAnalysis(),
                new CostAnalysis()
        };

        heapanalyses = new HeapDomain[] {
                GuiRunner.createNonRelationalMayHeapDomain(new TopHeapIdentifier(null, null)),
                GuiRunner.createNonRelationalMayHeapDomain(new ClassHeapIdentifier(null, null)),
                GuiRunner.createNonRelationalMayHeapDomain((ProgramPointHeapIdentifier) new NullProgramPointHeapIdentifier(null, null, 0)),
                GuiRunner.createNonRelationalMayAndMustHeapDomain((ProgramPointHeapIdentifier)new NullProgramPointHeapIdentifier(null, null, 0)),
                GuiRunner.createNonRelationalSummaryCollectionHeapDomain((ProgramPointHeapIdentifier)new NullProgramPointHeapIdentifier(null, null, 0)),
                new TVSHeap()
        };

    }
}
