package ch.ethz.inf.pm.sample.userinterfaces;

import ch.ethz.inf.pm.sample.abstractdomain.HeapDomain;
import ch.ethz.inf.pm.sample.abstractdomain.MaybeHeapIdSetDomain;
import ch.ethz.inf.pm.sample.abstractdomain.SemanticAnalysis;
import ch.ethz.inf.pm.sample.abstractdomain.accesspermissions.AccessPermissionsAnalysis;
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis.*;
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.ApronAnalysis;
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.NonRelationalNumericalAnalysis;
import ch.ethz.inf.pm.sample.oorepresentation.Type;
import ch.ethz.inf.pm.sample.oorepresentation.scalalang.ScalaCompiler;
import ch.ethz.inf.pm.sample.td.cost.loops.CostAnalysis;
import ch.ethz.inf.pm.sample.td.cost.loops.LoopCostCompiler;
import ch.ethz.inf.pm.td.analysis.TouchAnalysis;
import ch.ethz.inf.pm.td.analysis.TouchAnalysisWithApron;
import ch.ethz.inf.pm.td.compiler.TouchCompiler;
import it.unive.dsi.stringanalysis.BricksAnalysis;
import it.unive.dsi.stringanalysis.PrefixAndSuffixAnalysis;
import it.unive.dsi.stringanalysis.SurelyAndMaybeContainedCharactersAnalysis;

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
                new TouchAnalysisWithApron(),
                new CostAnalysis()
        };

        heapanalyses = new HeapDomain[] {
                createNonRelationalMayHeapDomain(new TopHeapIdentifier(null, null)),
                createNonRelationalMayHeapDomain(new ClassHeapIdentifier(null, null)),
                createNonRelationalMayHeapDomain(new NullProgramPointHeapIdentifier(null, null, 0)),
                createNonRelationalMayAndMustHeapDomain(new NullProgramPointHeapIdentifier(null, null, 0)),
                createNonRelationalSummaryCollectionHeapDomain(new NullProgramPointHeapIdentifier(null, null, 0)),
                new TVSHeap()
        };

    }

    private static NonRelationalHeapDomain createNonRelationalMayHeapDomain(NonRelationalHeapIdentifier id) {
        Type typ=null;
        MaybeHeapIdSetDomain ids = new MaybeHeapIdSetDomain();
        VariableEnv env= new VariableEnv(typ, ids);
        HeapEnv heap= new HeapEnv(typ, ids);
        return new NonRelationalHeapDomain(env, heap, ids, id);
    }

    private static NonRelationalMayAndMustHeapDomain createNonRelationalMayAndMustHeapDomain(NonRelationalHeapIdentifier id) {
        Type typ=null;
        NonRelationalHeapDomain mayHeap = createNonRelationalMayHeapDomain(id);

        TupleIdSetDomain mustIds = new TupleIdSetDomain();
        VariableEnv mustEnv = new VariableEnv(typ, mustIds);
        HeapEnv mustHeapEnv = new HeapEnv(typ, mustIds);
        NonRelationalMustHeapDomain mustHeap = new NonRelationalMustHeapDomain(mustEnv, mustHeapEnv, mustIds, id);

        return new NonRelationalMayAndMustHeapDomain(mayHeap, mustHeap);
    }

    private static NonRelationalSummaryCollectionHeapDomain createNonRelationalSummaryCollectionHeapDomain(NonRelationalHeapIdentifier id) {
        Type typ=null;
        MaybeHeapIdSetDomain ids = new MaybeHeapIdSetDomain();
        VariableEnv env= new VariableEnv(typ, ids);
        HeapEnv heap= new HeapEnv(typ, ids);
        return new NonRelationalSummaryCollectionHeapDomain(env, heap, ids, id);
    }
}
