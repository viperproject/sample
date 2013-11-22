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
        compilers=new ch.ethz.inf.pm.sample.oorepresentation.Compiler[3];
        compilers[0]=new ScalaCompiler();
        compilers[1]=new TouchCompiler();
        compilers[2]=new LoopCostCompiler();

        analyses=new SemanticAnalysis[9];
        analyses[0]=new NonRelationalNumericalAnalysis();
        analyses[1]=new AccessPermissionsAnalysis();
        analyses[2]=new ApronAnalysis();
        analyses[3]=new PrefixAndSuffixAnalysis();
        analyses[4]=new BricksAnalysis();
        analyses[5]=new SurelyAndMaybeContainedCharactersAnalysis();
        analyses[6]=new TouchAnalysis();
        analyses[7]=new TouchAnalysisWithApron();
        analyses[8]=new CostAnalysis();

        heapanalyses=new HeapDomain[6];
        heapanalyses[0]=createNonRelationalMayHeapDomain(new TopHeapIdentifier(null, null));
        heapanalyses[1]=createNonRelationalMayHeapDomain(new ClassHeapIdentifier(null, null));
        heapanalyses[2]=createNonRelationalMayHeapDomain(new NullProgramPointHeapIdentifier(null, null, 0));
        heapanalyses[3]=createNonRelationalMayAndMustHeapDomain(new NullProgramPointHeapIdentifier(null, null, 0));
        heapanalyses[4]=createNonRelationalSummaryCollectionHeapDomain(new NullProgramPointHeapIdentifier(null, null, 0));
        heapanalyses[5]=new TVSHeap();

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
