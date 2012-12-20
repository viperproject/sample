package ch.ethz.inf.pm.sample.userinterfaces;
import ch.ethz.inf.pm.sample.SystemParameters;
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis.*;
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.ApronAnalysis;
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.NonRelationalNumericalAnalysis;
import ch.ethz.inf.pm.sample.oorepresentation.*;
import ch.ethz.inf.pm.sample.abstractdomain.*;
import ch.ethz.inf.pm.sample.abstractdomain.accesspermissions.*;
import ch.ethz.inf.pm.sample.oorepresentation.Compiler;
import ch.ethz.inf.pm.sample.oorepresentation.scalalang.*;
import ch.ethz.inf.pm.td.compiler.SimpleTouchCompiler;
import ch.ethz.inf.pm.td.compiler.TouchCompiler;
import ch.ethz.inf.pm.td.domain.TouchAnalysis;
import ch.ethz.inf.pm.td.analysis.TouchAnalysisWithApron;
import it.unive.dsi.stringanalysis.BricksAnalysis;
import it.unive.dsi.stringanalysis.PrefixAndSuffixAnalysis;
import it.unive.dsi.stringanalysis.SurelyAndMaybeContainedCharactersAnalysis;
import scala.collection.immutable.List;
import semper.sample.multithreading.AugmentedCompiler;
import semper.sample.multithreading.MultithreadingAnalysis;

import java.io.File;
import java.io.FileWriter;

public class InstalledPlugins {
    public static ch.ethz.inf.pm.sample.oorepresentation.Compiler[] compilers;
    public static SemanticAnalysis<?>[] analyses;
    public static HeapDomain<?, ?>[] heapanalyses;

    static  {
        compilers=new ch.ethz.inf.pm.sample.oorepresentation.Compiler[4];
        compilers[0]=new ScalaCompiler();
        compilers[1]=new TouchCompiler();
        compilers[2]=new SimpleTouchCompiler();
        compilers[3]=new AugmentedCompiler();

        analyses=new SemanticAnalysis[9];
        analyses[0]=new NonRelationalNumericalAnalysis();
        analyses[1]=new AccessPermissionsAnalysis();
        analyses[2]=new ApronAnalysis();
        analyses[3]=new PrefixAndSuffixAnalysis();
        analyses[4]=new BricksAnalysis();
        analyses[5]=new SurelyAndMaybeContainedCharactersAnalysis();
        analyses[6]=new TouchAnalysis();
        analyses[7]=new TouchAnalysisWithApron();
        analyses[8]=new MultithreadingAnalysis();

        heapanalyses=new HeapDomain[4];
        heapanalyses[0]=createNonRelationalHeapDomain(new TopHeapIdentifier(null, null));
        heapanalyses[1]=createNonRelationalHeapDomain(new ClassHeapIdentifier(null, null));
        heapanalyses[2]=createNonRelationalHeapDomain(new NullProgramPointHeapIdentifier(null, null));
        heapanalyses[3]=new TVSHeap();
    }

    private static NonRelationalHeapDomain createNonRelationalHeapDomain(NonRelationalHeapIdentifier id) {
        Type typ=null;
        MaybeHeapIdSetDomain ids = new MaybeHeapIdSetDomain();
        VariableEnv env= new VariableEnv(typ, ids);
        HeapEnv heap= new HeapEnv(typ, ids);
        return new NonRelationalHeapDomain(env, heap, ids, id);
    }

	static void generateTopType(Compiler c) throws Exception {

		if (c instanceof ScalaCompiler) {
            String suffix = ".scala";

            File file = File.createTempFile("Dummy", suffix);

            String className = file.getName().substring(0, file.getName().length() - suffix.length());
            String source = "class " + className + " {}";

            // Write source
            FileWriter out = new FileWriter(file);
            out.write(source);
            out.close();

            List<ClassDefinition> classes = c.compileFile(file.getAbsolutePath());
            if (classes.length() > 0) {
                SystemParameters.typ_$eq(classes.head().typ().top());
            } else {
                throw new Exception("Could not generate type information");
            }

            // Remove files
            File classFile = new File(className + ".class");
            if (classFile.exists()) classFile.delete();

        } else if (c instanceof TouchCompiler) {

            List<ClassDefinition> classes = ((TouchCompiler)c).compileString("","dummypub");
            if (classes.length() > 0) {
                SystemParameters.typ_$eq(classes.head().typ().top());
            } else {
                throw new Exception("Could not generate type information");
            }

        }
	}

}
