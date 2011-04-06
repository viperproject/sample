package ch.ethz.inf.pm.sample.userinterfaces;
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis.*;
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.ApronAnalysis;
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.NonRelationalNumericalAnalysis;
import ch.ethz.inf.pm.sample.oorepresentation.*;
import ch.ethz.inf.pm.sample.abstractdomain.*;
import ch.ethz.inf.pm.sample.abstractdomain.accesspermissions.*;
import ch.ethz.inf.pm.sample.oorepresentation.javabytecode.*;
import ch.ethz.inf.pm.sample.oorepresentation.scalalang.*;

public class InstalledPlugins {
    public static ch.ethz.inf.pm.sample.oorepresentation.Compiler[] compilers;
    public static Analysis[] analyses;
    public static HeapDomain[] heapanalyses;

    static  {
        compilers=new ch.ethz.inf.pm.sample.oorepresentation.Compiler[2];
        compilers[0]=new ScalaCompiler();
        compilers[1]=new JavaCompiler();

        analyses=new Analysis[3];
        analyses[0]=new AccessPermissionsAnalysis();
        analyses[1]=new NonRelationalNumericalAnalysis();
        analyses[2]=new ApronAnalysis();

        heapanalyses=new HeapDomain[5];
        heapanalyses[0]=new ReallyApproximatedHeapDomain();
        heapanalyses[1]=new OnlyStaticReferenceHeapDomain();
        heapanalyses[2]=createNonRelationalHeapDomain(new SingleHeapIdentifier(null));
        heapanalyses[3]=createNonRelationalHeapDomain(new ClassHeapIdentifier(null));
        heapanalyses[4]=createNonRelationalHeapDomain(new NullProgramPointHeapIdentifier(null));
    }

    private static NonRelationalHeapDomain createNonRelationalHeapDomain(NonRelationalHeapIdentifier id) {
        Type typ=null;
        HeapIdAndSetDomain ids = new HeapIdAndSetDomain(id);
        VariableEnv env= new VariableEnv(typ, ids);
        HeapEnv heap= new HeapEnv(typ, ids);
        return new NonRelationalHeapDomain(env, heap, ids, id);
    }

}
