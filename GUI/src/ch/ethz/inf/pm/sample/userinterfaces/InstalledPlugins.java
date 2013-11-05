package ch.ethz.inf.pm.sample.userinterfaces;
import ch.ethz.inf.pm.sample.td.cost.loops.*;
import ch.ethz.inf.pm.sample.SystemParameters;
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis.*;
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.ApronAnalysis;
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.NonRelationalNumericalAnalysis;
import ch.ethz.inf.pm.sample.oorepresentation.*;
import ch.ethz.inf.pm.sample.abstractdomain.*;
import ch.ethz.inf.pm.sample.abstractdomain.accesspermissions.*;
import ch.ethz.inf.pm.sample.oorepresentation.Compiler;
import ch.ethz.inf.pm.sample.oorepresentation.scalalang.*;
import ch.ethz.inf.pm.td.compiler.TouchCompiler;
import ch.ethz.inf.pm.td.analysis.*;
import ch.ethz.inf.pm.td.parser.Declaration;
import ch.ethz.inf.pm.td.parser.LibraryDefinition;
import ch.ethz.inf.pm.td.parser.Script;
import ch.ethz.inf.pm.td.webapi.*;
import it.unive.dsi.stringanalysis.BricksAnalysis;
import it.unive.dsi.stringanalysis.PrefixAndSuffixAnalysis;
import it.unive.dsi.stringanalysis.SurelyAndMaybeContainedCharactersAnalysis;
import scala.collection.immutable.*;
import scala.Option;
import semper.sample.multithreading.AugmentedCompiler;
import semper.sample.multithreading.MultithreadingAnalysis;

import java.io.File;
import java.io.FileWriter;
import java.util.GregorianCalendar;

public class InstalledPlugins {
    public static ch.ethz.inf.pm.sample.oorepresentation.Compiler[] compilers;
    public static SemanticAnalysis<?>[] analyses;
    public static HeapDomain<?, ?>[] heapanalyses;
    public static IteratorOverPrograms[] iterators;

    static  {
        compilers=new ch.ethz.inf.pm.sample.oorepresentation.Compiler[4];
        compilers[0]=new ScalaCompiler();
        compilers[1]=new TouchCompiler();
        compilers[2]=new AugmentedCompiler();
        compilers[3]=new LoopCostCompiler();

        analyses=new SemanticAnalysis[10];
        analyses[0]=new NonRelationalNumericalAnalysis();
        analyses[1]=new AccessPermissionsAnalysis();
        analyses[2]=new ApronAnalysis();
        analyses[3]=new PrefixAndSuffixAnalysis();
        analyses[4]=new BricksAnalysis();
        analyses[5]=new SurelyAndMaybeContainedCharactersAnalysis();
        analyses[6]=new TouchAnalysis();
        analyses[7]=new TouchAnalysisWithApron();
        analyses[8]=new MultithreadingAnalysis();
        analyses[9]=new CostAnalysis();

        heapanalyses=new HeapDomain[6];
        heapanalyses[0]=createNonRelationalMayHeapDomain(new TopHeapIdentifier(null, null));
        heapanalyses[1]=createNonRelationalMayHeapDomain(new ClassHeapIdentifier(null, null));
        heapanalyses[2]=createNonRelationalMayHeapDomain(new NullProgramPointHeapIdentifier(null, null, 0));
        heapanalyses[3]=createNonRelationalMayAndMustHeapDomain(new NullProgramPointHeapIdentifier(null, null, 0));
        heapanalyses[4]=createNonRelationalSummaryCollectionHeapDomain(new NullProgramPointHeapIdentifier(null, null, 0));
        heapanalyses[5]=new TVSHeap();


        iterators = new IteratorOverPrograms[15];
        iterators[0] = new TopScripts();
        iterators[1] = new NewScripts();
        iterators[2] = new FeaturedScripts();
        iterators[3] = new RootScripts();
        iterators[4] = new SampleScript();
        iterators[5] = new RootScriptsWithLoops();
        iterators[6] = new RootSampleScriptsWithLoops();
        iterators[7] = new TopRootScriptsWithLoops();
        iterators[8] = new ScriptsWithLoops();
        iterators[9] = new ScriptListings();
        iterators[10] = new ReadIdsFromFile("Test/test/TouchDevelop/testsets/131101_R","131101_R");
        iterators[11] = new ReadIdsFromFile("Test/test/TouchDevelop/testsets/A_131101","A_131101");
        iterators[12] = new ReadIdsFromFile("Test/test/TouchDevelop/testsets/AA_131101","AA_131101");
        iterators[13] = new ReadIdsFromFile("Test/test/TouchDevelop/testsets/TOP_131101","TOP_131101");
        iterators[14] = new ReadIdsFromFile("Test/test/TouchDevelop/testsets/TOP_131101_NR","TOP_131101_NR");

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
